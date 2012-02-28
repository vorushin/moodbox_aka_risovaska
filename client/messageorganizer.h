#ifndef MESSAGEORGANIZER_H
#define MESSAGEORGANIZER_H

#include "messagekey.h"
#include "messagetype.h"

namespace MoodBox
{

#define MESSAGES_OUTBOX_FOLDER				"Outbox"
#define MESSAGES_REJECTED_FOLDER			"Rejected"
#define MESSAGES_HISTORY_FOLDER				"History"
#define MESSAGES_HISTORY_FRIENDS_FOLDER		"Friends"

#define MESSAGES_PREVIEW_FILE_EXT			".png"

#define MESSAGE_THUMBNAIL_WIDTH				111
#define MESSAGE_THUMBNAIL_HEIGHT			69
#define MESSAGES_THUMBNAIL_SUFFIX			"_thumbnail"

// Static class for message folders and names management
class MessageOrganizer
{
public:
	// Folders
	static QString getHistoryFolder();
	static QString getContactHistoryFolder(qint32 contactId);
	static QString getFriendsHistoryFolder();
	static QString getOutboxFolder();
	static QString getRejectedFolder();

	static MessageKey getKey(const QString &fileName);
	static MessageKey findKey(const qint32 messageId, MessageType::MessageTypeEnum messageType, qint32 contactId);

	// New file name
	static QString getFileName(const MessageKey &key);
	static QString getFilePath(const MessageKey &key, MessageType::MessageTypeEnum messageType, qint32 contactId);

	// Get preview file name from message file name
	static QString getPreviewFileName(const MessageKey &key, MessageType::MessageTypeEnum messageType, qint32 contactId);
	static QString getPreviewFileName(const QString &messageFileName);

	// Get thumbnail file name from preview file name
	static QString getThumbnailFileName(const QString &previewFileName);

	// Remove message, preview and thumbnail files
	static bool removeFiles(const MessageKey &key, MessageType::MessageTypeEnum messageType, qint32 contactId);
	static bool removeFiles(const QString &messageFilePath);

	// Paths
	static QString getPrivateMessageFileName(qint32 contactId, const QString &newFileName);
	static QString getFriendsMessageFileName(const QString &newFileName);
	static QString getOutboxFileName(const QString &newFileName);
	static QString getRejectedFileName(const QString &newFileName);

private:
	static QString getContactHistoryFolder(qint32 contactId, bool ensureExisting);
};

}

#endif // MESSAGEORGANIZER_H