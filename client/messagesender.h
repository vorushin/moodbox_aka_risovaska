#ifndef MESSAGESENDER_H
#define MESSAGESENDER_H

#include "timerobjects.h"

#include <QMap>

#include "messagekey.h"
#include "fault.h"
#include "sendmessageresult.h"
#include "userstatus.h"

namespace MoodBox
{

// Send timer maximum interval in msecs
#define SENDER_RETRY_MAXIMUM_INTERVAL	60000

// Send timer interval in msecs
#define SENDER_RETRY_INTERVAL			5000

// Send repeats if rejected
#define SENDER_REJECTED_RETRIES			2

// Send network repeats if rejected
#define NETWORK_REJECTED_RETRIES		5

class MessageFile;

// Message queued sender
class MessageSender : public RandomRetryTimerObject
{
	Q_OBJECT

public:
	MessageSender(QObject *parent = 0);
	virtual ~MessageSender();

	void addToQueue(MessageFile *file);
	void addToQueue(const QList<MessageFile *> &files);

	bool removeFromQueue(const MessageKey &key);

	void init();
	void start();
	void stop();
	void cleanup();

signals:
	// Completion signals
	void privateMessageSent(qint32 recipientId, const MessageKey &newKey, ContactResultCode::ContactResultCodeEnum result, const MessageKey &oldKey);
	void friendsMessageSent(const MessageKey &newKey, ContactResultCode::ContactResultCodeEnum result, const MessageKey &oldKey);
	
	// Error signals
	void sendError();

	void showExceptionDialogSendingMessage(ContactResultCode::ContactResultCodeEnum ResultCode, qint32 recipientId);

private:
	/* Sender's states:
		WaitForFiles - nothing to do so far
		WaitForTimer - waiting for send timer to timeout
		WaitForServer - waiting for server response

	   State change drivers:
		1. User Account started - need to load outbox files to queue and wait for user to become Online to send them
		2. Added files to queue - need to start job if not yet started
		3. User status is Online - can start/resume sending queued files one by one to server
		4. Send timer timeout - send file to server and wait for sent response
		5. Got server sent response - need to update sent file and remove it from outbox and queue
		6. User status is not Online - need to pause job (e.g. network error)
		7. User Account stopped - we need to pause job and clear queue except the processing file, if any

	   Sender workflow:				   
		WaitForFiles -> Sending <-> WaitForServer -> WaitForFiles
	*/

	enum State { WaitForFiles, Sending, WaitForServer };
	State state;

	bool paused;
	qint32 serverErrorsCount;

	QMap <MessageKey, MessageFile*> sendQueue;

	// Events
	void gotInit();
	void gotFilesAdded();
	void gotStarted();
	void gotStopped();
	void gotTimerTimeout();
	void gotServerResponse();
	void gotCleanup();
	void gotNetworkError(Fault fault, const MessageKey &key);
	void gotServerError(Fault fault, const MessageKey &key);

	// State transitions
	void goSending();
	void goSendFile();
	void goWaitForFiles();
	void goPaused();
	void goContinue();
	void goStop();

	// Send operations
    // Load files from outbox
	void loadOutbox();

	// Send message file to server
	void sendFile(const MessageKey &key);

	// Clear all queue except first one
	void cancelQueue();

	// Clear all tries counters
	void resetAllTries();

	// Message sent completed
	void fileSent(const MessageKey &key, const SendMessageResult &result);

	// Could not send file
	void rejectFile(Fault fault, const MessageKey &key);

	// Save updated file
	void updateMessageFile(MessageFile *file, const MessageKey &oldKey);

	// Sent notification
	void sendSentNotification(MessageFile *file, ContactResultCode::ContactResultCodeEnum result, const MessageKey &oldKey);

	// Network error occured
	void networkError(Fault fault, const MessageKey &key);

	// Server error occured
	void serverError(Fault fault, const MessageKey &key);

private slots:
	void onTimerTimeout();

	void onSendMessageResult(QVariant state, Fault fault, SendMessageResult result);
};

}

#endif // MESSAGESENDER_H