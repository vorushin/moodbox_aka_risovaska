#include "messageorganizer.h"

#include <QDir>
#include <QFile>

#include "peopleinfomanager.h"
#include "messagefile.h"
#include "apptools.h"

namespace MoodBox
{

QString makePathIfNeeded(const QString &path)
{
	QDir dir(path);

	if (!dir.exists())
		dir.mkpath(path);

	return path;
}

QString MessageOrganizer::getHistoryFolder()
{
	QString userFolder = INFOMANAGER->getUserSettingsFolder();

	return makePathIfNeeded(AppTools::addPathSeparator(userFolder) + MESSAGES_HISTORY_FOLDER);
}

QString MessageOrganizer::getContactHistoryFolder(qint32 contactId)
{
	return getContactHistoryFolder(contactId, true);
}

QString MessageOrganizer::getFriendsHistoryFolder()
{
	return makePathIfNeeded(AppTools::addPathSeparator(getHistoryFolder()) + MESSAGES_HISTORY_FRIENDS_FOLDER);
}

QString MessageOrganizer::getOutboxFolder()
{
	return makePathIfNeeded(AppTools::addPathSeparator(getHistoryFolder()) + MESSAGES_OUTBOX_FOLDER);
}

QString MessageOrganizer::getRejectedFolder()
{
	return makePathIfNeeded(AppTools::addPathSeparator(getOutboxFolder()) + MESSAGES_REJECTED_FOLDER);
}

MessageKey MessageOrganizer::getKey(const QString &fileName)
{
	static const int TimestampLength = strlen(MESSAGE_KEY_TIMESTAMP);

	QDateTime dateTime = QDateTime::fromString(fileName.mid(fileName.lastIndexOf('/') + 1, TimestampLength), MESSAGE_KEY_TIMESTAMP);
	dateTime.setTimeSpec(Qt::UTC);

	int index = fileName.lastIndexOf(MESSAGE_KEY_SEPARATOR) + 1;
	qint32 id = fileName.mid(index, fileName.length() - index - 4).toInt();

	return MessageKey(dateTime, id);
}

MessageKey MessageOrganizer::findKey(const qint32 messageId, MessageType::MessageTypeEnum messageType, qint32 contactId)
{
	QDir dir;

	if (messageType == MessageType::Friends)
		dir = QDir(getFriendsHistoryFolder(), "*_" + QString::number(messageId) + MESSAGE_FILE_EXTENSION, QDir::NoSort, QDir::Files);
	else
		dir = QDir(getContactHistoryFolder(contactId), "*_" + QString::number(messageId) + MESSAGE_FILE_EXTENSION, QDir::NoSort, QDir::Files);

 	QFileInfoList list = dir.entryInfoList();

	if (!list.isEmpty())
	{
		QFileInfo fileInfo = list.at(0);
		return MessageOrganizer::getKey(fileInfo.fileName());
    }

	return MessageKey();
}

QString MessageOrganizer::getFileName(const MessageKey &key)
{
	return key.toString() + MESSAGE_FILE_EXTENSION;
}

QString MessageOrganizer::getFilePath(const MessageKey &key, MessageType::MessageTypeEnum messageType, qint32 contactId)
{
	QString fileName = getFileName(key);

	if(messageType == MessageType::Friends)
		return MessageOrganizer::getFriendsMessageFileName(fileName);

	return MessageOrganizer::getPrivateMessageFileName(contactId, fileName);
}

QString MessageOrganizer::getPreviewFileName(const MessageKey &key, MessageType::MessageTypeEnum messageType, qint32 contactId)
{
	return getPreviewFileName(getFilePath(key, messageType, contactId));
}

QString MessageOrganizer::getPreviewFileName(const QString &messageFileName)
{
	static const int MessageExtLen = QString(MESSAGE_FILE_EXTENSION).length();

	QString previewName = messageFileName;

	previewName.chop(MessageExtLen);
	previewName += MESSAGES_PREVIEW_FILE_EXT;

	return previewName;
}

QString MessageOrganizer::getThumbnailFileName(const QString &previewFileName)
{
	int insertPos = previewFileName.lastIndexOf(MESSAGES_PREVIEW_FILE_EXT);

	QString thumbnailName = previewFileName;

	thumbnailName.insert(insertPos, MESSAGES_THUMBNAIL_SUFFIX);

	return thumbnailName;
}

bool MessageOrganizer::removeFiles(const MessageKey &key, MessageType::MessageTypeEnum messageType, qint32 contactId)
{
	return removeFiles(getFilePath(key, messageType, contactId));
}

bool MessageOrganizer::removeFiles(const QString &messageFilePath)
{
	const bool existed = QFile::exists(messageFilePath);

	const bool removed = QFile::remove(messageFilePath) && existed;

	QString previewFileName = getPreviewFileName(messageFilePath);

	QFile::remove(previewFileName);
	QFile::remove(getThumbnailFileName(previewFileName));

	return removed;
}

QString MessageOrganizer::getPrivateMessageFileName(qint32 contactId, const QString &newFileName)
{
	return AppTools::addPathSeparator(getContactHistoryFolder(contactId)) + newFileName;
}

QString MessageOrganizer::getFriendsMessageFileName(const QString &newFileName)
{
	return AppTools::addPathSeparator(getFriendsHistoryFolder()) + newFileName;
}

QString MessageOrganizer::getOutboxFileName(const QString &newFileName)
{
	return AppTools::addPathSeparator(getOutboxFolder()) + newFileName;
}

QString MessageOrganizer::getRejectedFileName(const QString &newFileName)
{
	return AppTools::addPathSeparator(getRejectedFolder()) + newFileName;
}

QString MessageOrganizer::getContactHistoryFolder(qint32 contactId, bool ensureExisting)
{
	QString folder = AppTools::addPathSeparator(getHistoryFolder()) + QString::number(contactId);

	return (ensureExisting) ? makePathIfNeeded(folder) : folder;
}

}