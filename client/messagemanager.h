#ifndef MESSAGEMANAGER_H
#define MESSAGEMANAGER_H

#include <QObject>
#include <QList>
#include <QImage>

#include "messagekey.h"
#include "messagetype.h"
#include "contactresultcode.h"
#include "notification.h"
#include "userstatus.h"
#include "publishingway.h"

#include "publishmessageinfo.h"

namespace MoodBox
{

#define MESSAGEMANAGER						MessageManager::getInstance()

class MessageSender;
class MessageReceiver;
class ServerCommandsReceiver;
class MessagePublisher;
class MessageFile;

// Messages manager, responsible for sending and receiving of art messages
class MessageManager : public QObject
{
	Q_OBJECT

public:
	// Init all components
	void initMessaging();

	// Start & stop processing
	void startMessageProcessing();
	void stopMessageProcessing();

	// Cleanup and stop
	void cleanupMessaging();
	
	// Operations: send, publish, etc
	void sendMessage(MessageFile *file);
	
	void publishMoodstrip(qint32 id, const QString &caption, const QList <PublishMessageInfo> &messages, bool isHidden, qint32 recipientId);
	void cancelPublish(qint32 id);

	QString saveThumbnail(const QString &previewFileName, const QImage &preview);

	static MessageManager *getInstance();

	void receiveMessage(MessageFile *file);

	bool deleteMessage(const MessageKey &key, MessageType::MessageTypeEnum messageType, qint32 recipientId);
	bool deleteMessage(const qint32 messageId, MessageType::MessageTypeEnum messageType, qint32 recipientId);

signals:
	// Initial send signal
	void privateMessageSent(qint32 recipientId, const MessageKey &key);
	void friendsMessageSent(const MessageKey &key);

	// Send Info update
	void privateMessageSentUpdate(qint32 recipientId, const MessageKey &newKey, ContactResultCode::ContactResultCodeEnum result, const MessageKey &oldKey);
	void friendsMessageSentUpdate(const MessageKey &newKey, ContactResultCode::ContactResultCodeEnum result, const MessageKey &oldKey);

	// Send error
	void sendError();
	void showExceptionDialogSendingMessage(ContactResultCode::ContactResultCodeEnum ResultCode, qint32 id);

	// Receive
	void privateMessageReceived(qint32 authorId, const MessageKey &key);
	void channelMessageReceived(qint32 channelId, const MessageKey &key);
	void friendsMessageReceived(const MessageKey &key);

	// Publish
	void publishing(qint32 id, qint32 percentDone);
	void publishCompleted(qint32 id, qint32 moodstripId, const QList<PublishingWay> &urls);
	
	void publishError(qint32 id);

	// Delete
	void messageDeleted(const MessageKey &key);

private:
	MessageSender *sender;
	MessageReceiver *receiver;
	ServerCommandsReceiver *commandReceiver;
	MessagePublisher *publisher;

	MessageManager();
		
	static MessageManager *instance;

private slots:
	void onMessageReceived(MessageFile *file);
};

}

#endif // MESSAGEMANAGER_H
