#include "messagemanager.h"

#include <QFile>

#include "peopleinfomanager.h"
#include "serverproxysingleton.h"

#include "messagefile.h"
#include "messageorganizer.h"
#include "messagesender.h"
#include "messagereceiver.h"
#include "servercommandreceiver.h"
#include "messagepublisher.h"
#include "channelinfomanager.h"

#include "common.h"
#include "imageelement.h"
#include "debug.h"

namespace MoodBox
{

MessageManager* MessageManager::instance = NULL;

MessageManager::MessageManager()
	: QObject()
{	
	// Sender init
	sender = new MessageSender(this);
	connect(sender, SIGNAL(privateMessageSent(qint32, const MessageKey &, ContactResultCode::ContactResultCodeEnum, const MessageKey &)), this, SIGNAL(privateMessageSentUpdate(qint32, const MessageKey &, ContactResultCode::ContactResultCodeEnum, const MessageKey &)));
	connect(sender, SIGNAL(friendsMessageSent(const MessageKey &, ContactResultCode::ContactResultCodeEnum, const MessageKey &)), this, SIGNAL(friendsMessageSentUpdate(const MessageKey &, ContactResultCode::ContactResultCodeEnum, const MessageKey &)));
	connect(sender, SIGNAL(sendError()), this, SIGNAL(sendError()));
	connect(sender, SIGNAL(showExceptionDialogSendingMessage(ContactResultCode::ContactResultCodeEnum, qint32)), this, SIGNAL(showExceptionDialogSendingMessage(ContactResultCode::ContactResultCodeEnum, qint32)));

	// Receiver init
	receiver = new MessageReceiver(this);
	connect(receiver, SIGNAL(messageReceived(MessageFile *)), this, SLOT(onMessageReceived(MessageFile *)));

	// Command receiver init
	commandReceiver = new ServerCommandsReceiver();

	// Publisher init
	publisher = new MessagePublisher(this);

	connect(publisher, SIGNAL(publishing(qint32, qint32)), this, SIGNAL(publishing(qint32, qint32)));
	
	connect(publisher, SIGNAL(publishCompleted(qint32, qint32, const QList <PublishingWay> &)), this, SIGNAL(publishCompleted(qint32, qint32, const QList <PublishingWay> &)));
	connect(publisher, SIGNAL(publishError(qint32)), this, SIGNAL(publishError(qint32)));
}

void MessageManager::initMessaging()
{
	sender->init();
}

void MessageManager::startMessageProcessing()
{
	sender->start();
	receiver->start();
	commandReceiver->start();
	publisher->start();
}

void MessageManager::stopMessageProcessing()
{
	sender->stop();
	receiver->stop();
	commandReceiver->stop();
	publisher->stop();
}

void MessageManager::cleanupMessaging()
{
	sender->cleanup();
	publisher->cleanup();
}

void MessageManager::sendMessage(MessageFile *file)
{
	// Set author
	file->setAuthor(INFOMANAGER->getUserAccount().getId());
	file->setAuthorLogin(INFOMANAGER->getUserAccount().getLogin());

	// Get file name
	MessageKey key;
	const QString fileName = MessageOrganizer::getFileName(key);
	file->setSentDate(key.getDate());

	// POSTPONED: Temporary solution, should be replaced
	bool isSandBox = file->getRecipient() == file->getAuthor();

	QString outboxName;

	// Save to outbox
	if (!isSandBox)
	{
		outboxName = MessageOrganizer::getOutboxFileName(fileName);
		file->setFileName(outboxName);
		file->save();
	}

	// Save history info
	QString historyName;

	switch (file->getMessageType())
	{
		case MessageType::Private:
		case MessageType::Channel:
			historyName = MessageOrganizer::getPrivateMessageFileName(file->getRecipient(), fileName);
			break;

		case MessageType::Friends:
			historyName = MessageOrganizer::getFriendsMessageFileName(fileName);
			break;

		default:
			Q_ASSERT_X(false, "MessageManager::sendMessage", "Not supported file type");
	
	}
	
	// Write history
	if (!isSandBox)
	{
		QFile::copy(outboxName, historyName);
	}
	else
	{
		file->setSent(true);
		file->setFileName(historyName);
		file->save();
	}

	// Save preview
	QImage previewImage = file->getPreview();

	// TODO: replace to universal load from DB. Making WYSIWYG image
	QString previewFormat = file->getImageFormatFromInfo();
	if (previewFormat != DEFAULT_IMAGE_FORMAT)
	{
		QByteArray format = previewFormat.toLatin1();
		previewImage.loadFromData(file->getPreviewBytes(), format.data());
	}

	QString previewName = MessageOrganizer::getPreviewFileName(historyName);
	previewImage.save(previewName);

	if (!isSandBox)
	{
		QString outboxPreviewName = MessageOrganizer::getPreviewFileName(outboxName);
		previewImage.save(outboxPreviewName);
	}

	// Save thumbnail
	saveThumbnail(previewName, previewImage);

	// Add to sender queue
	if (!isSandBox)
		sender->addToQueue(file);

	// Notify
	switch (file->getMessageType())
	{
		case MessageType::Private:
		case MessageType::Channel:
			QDEBUG("Private message sent to " << file->getRecipient());
			emit privateMessageSent(file->getRecipient(), file->getMessageKey());
			break;

		case MessageType::Friends:
			QDEBUG("Friend message sent");
			emit friendsMessageSent(file->getMessageKey());
			break;

		default:
			Q_ASSERT_X(false, "MessageManager::sendMessage", "Not supported file type");
	}

	// Clean up
	if (isSandBox)
		delete file;
}

void MessageManager::publishMoodstrip(qint32 id, const QString &caption, const QList <PublishMessageInfo> &messages, bool isHidden, qint32 recipientId)
{
	publisher->publish(id, caption, messages, isHidden, recipientId);
}

void MessageManager::cancelPublish(qint32 id)
{
	publisher->cancel(id);
}

QString MessageManager::saveThumbnail(const QString &previewFileName, const QImage &preview)
{
	QString thumbnailFileName = MessageOrganizer::getThumbnailFileName(previewFileName);
	preview.scaled(MESSAGE_THUMBNAIL_WIDTH, MESSAGE_THUMBNAIL_HEIGHT).save(thumbnailFileName);

	return thumbnailFileName;
}

MessageManager *MessageManager::getInstance()
{
	if (instance == NULL )
		instance = new MessageManager();

	return instance;
}

void MessageManager::receiveMessage(MessageFile *file)
{
	if (file->getMessageType() != MessageType::Channel)
	{
		ContactInfo *authorContact = INFOMANAGER->getContact(file->getAuthor());

		if (authorContact != NULL && file->getAuthorLogin().isEmpty())
			file->setAuthorLogin(authorContact->getLogin());
	}

	// Get file name
	QString fileName = MessageOrganizer::getFileName(file->getMessageKey());

	// Save history info
	QString historyName;

	switch (file->getMessageType())
	{
		case MessageType::Channel:
			historyName = MessageOrganizer::getPrivateMessageFileName(file->getRecipient(), fileName);

			// increase last message id for channel
			CHANNELMANAGER->setLastChannelMessage(file->getRecipient(), file->getMessageKey().getId());
			break;

		case MessageType::Private:
			historyName = MessageOrganizer::getPrivateMessageFileName(file->getAuthor(), fileName);

			break;

		case MessageType::Friends:
			historyName = MessageOrganizer::getFriendsMessageFileName(fileName);
			break;

		default:
			Q_ASSERT_X(false, "MessageManager::receiveMessage", "Not supported file type");	
	}

	file->setFileName(historyName);
	file->save();

	QString previewFileName = MessageOrganizer::getPreviewFileName(historyName);
	file->getPreview().save(previewFileName);

	switch (file->getMessageType())
	{
		case MessageType::Channel:
			// Only if not from current user
			if (file->getAuthor() != INFOMANAGER->getUserAccount().getId())
				emit channelMessageReceived(file->getRecipient(), file->getMessageKey());
			break;

		case MessageType::Private:
			emit privateMessageReceived(file->getAuthor(), file->getMessageKey());
			break;

		case MessageType::Friends:
			emit friendsMessageReceived(file->getMessageKey());
			break;
	}

	delete file;
}

bool MessageManager::deleteMessage(const MessageKey &key, MessageType::MessageTypeEnum messageType, qint32 recipientId)
{
	if (!sender->removeFromQueue(key))
		return false;

	if (MessageOrganizer::removeFiles(key, messageType, recipientId))
	{
		emit messageDeleted(key);
		return true;
	}

	return false;
}

bool MessageManager::deleteMessage(const qint32 messageId, MessageType::MessageTypeEnum messageType, qint32 recipientId)
{
	MessageKey key = MessageOrganizer::findKey(messageId, messageType, recipientId);
	
	return (key.isValid()) ? deleteMessage(key, messageType, recipientId) : false;
}

void MessageManager::onMessageReceived(MessageFile *file)
{
	receiveMessage(file);
}

}