#ifndef MESSAGERECEIVER_H
#define MESSAGERECEIVER_H

#include <QQueue>

#include "timerobjects.h"

#include "fault.h"
#include "artmessage.h"
#include "channelmessage.h"
#include "channelmessageurl.h"
#include "notification.h"
#include "okresultcode.h"
#include "userstatus.h"

class QNetworkReply;

namespace MoodBox
{

// Maximum timeout for retries is 1 min (1 * 60 * 1000 msec)
#define RECEIVER_RETRY_MAXIMUM_INTERVAL				60000

// Receiver timeout for retries is 5 sec
#define RECEIVER_RETRY_INTERVAL						5000

// Periodic messages check interval is 10 mins ( 10 * 60 * 1000 )
#define RECEIVER_PERIODIC_UPDATE_INTERVAL			600000

// Channels update after contact list change
#define RECEIVER_CHANNELS_UPDATE_INTERVAL			20000

// Maximum channel messages count for 1 session
#define RECEIVER_MAXIMUM_CHANNEL_MESSAGES_SESSION	10

enum MessageModeEnum { SourceUndefined = 0, ArtMessages = 1, ChannelMessages = 2 };

struct MessageSource
{
	MessageModeEnum mode;
	int channelId;

	MessageSource() : mode(SourceUndefined), channelId(-1) {};
	bool operator== (const MessageSource & other ) const { return mode == other.mode && channelId == other.channelId; };
	MessageSource(MessageModeEnum mode, int channelId) { this->mode = mode; this->channelId = channelId; };
};

// Class to manage receiving messages (Art and Channel messages)
class RevolverRecieverManager : public SingleShotTimerObject
{
	Q_OBJECT

public:
	RevolverRecieverManager(QObject *parent = 0);

	void start();
	void putSourceInQueue(MessageSource source);
	MessageSource getNextMessageSource();
	MessageSource getNextMessageSourceCycled();

signals:
	void channelsChanged();

protected slots:
	virtual void onTimerTimeout();

public slots:
	void onContactListChanged();

private:
   QHash<qint32, MessageSource> sources;

   QQueue<MessageSource> requestQueue;

   int currentSourceIndex;

   void resetIndex();
};

class MessageFile;
class HttpLoader;

// Class to receive messages
class GetNextMessageRequest : public RandomRetryTimerObject
{
	Q_OBJECT

public:
	GetNextMessageRequest(QObject *parent = 0);
	~GetNextMessageRequest();
	
	void setSource(MessageSource source) { this->source = source; };

	void start() { gotStarting(); };
	void stop() { gotStopping(); };
	void setLastMessageId(int lastMessageId) { this->lastMessageId = lastMessageId; };

signals:
	void gotNextArtMessage(const ArtMessage &message);
	void gotNextChannelMessage(const ChannelMessage &message, qint32 channelId);

protected:
	/* Request states:
	    NotStarted - does nothing atm
		WaitForServer - wait for server response
		WaitForTimer - wait for retry timer
		WaitForHttp - wait for HTTP response

	  Request workflow:
	   [NotStarted] ->	Send Request ->	[WaitForServer] ->	Got good result -> [NotStarted] | -> [WaitForHttp] -> Not started
															Got bad result -> [WaitForTimer] -> Send request

	  If request should be stopped it should wait for server response and does not start timer after that
	*/

	enum State { NotStarted, WaitForServer, WaitForTimer, WaitForHttp };
	State state;

	MessageSource source;
	int lastMessageId;
	
	QString url;
	HttpLoader *http;

	bool stopRequested;
	bool skipMessage;

	// Events
	void gotStarting();
	void gotStopping();
	void gotRequestCompleted();
	void gotRequestResend();
	void gotUrl();
	void gotTimerTimeout();

	// State changes
	void goSend();
	void goStop();
	void goComplete();
	void goResend();
	void goLoadUrl();
	void goFinish();	

	void sendRequest();
	void loadUrl();
	void complete();

protected slots:
	virtual void onTimerTimeout();

private:
	ArtMessage artMessage;
	ChannelMessage channelMessage;
	
	void gotRetry();

private slots:
	void onGetNextArtMessageResult(QVariant state, Fault fault, ArtMessage result);

	void onGetNextChannelMessageUrlResult(QVariant state, Fault fault, ChannelMessageUrl result);
	
	void onGetNetworkReply(QNetworkReply *reply);
};

// Class for receive messages from server
class MessageReceiver : public RandomRetryTimerObject
{
	Q_OBJECT

public:
	MessageReceiver(QObject *parent = 0);
	virtual ~MessageReceiver();

	void start();
	void stop();

signals:
	void messageReceived(MessageFile *message);

private:
	/* Receiver states:
		WaitForTimer - waits for receiver auto-timer
		WaitForNextMessage - waits for next message request to be completed

	  Receiver workflow:
	   [Paused] -> User online -> [WaitForNextMessage] -> Got message? -> signal, [WaitForNextMessage]
																	   -> [WaitForTimer] -> [WaitForNextMessage]

      Receiver events:
	   1. User is online - unpause and start request for messages
	   2. User is offline - pause
	   3. Got notification about new messages - start request for messages
	   4. Timer timeout - start request for messages
	   5. Got no messages - start timer
	   6. Reload event - do retry for next message request, if started, or go start request for new messages
	   7. Got new message - need to notify about it
	*/

	enum State { WaitForTimer, WaitForNextMessage };
	State state;

	MessageSource currentSource;
	bool hadChannelMessages;
	int channelMessagesCount;
	
	bool paused;

	bool isProcessingNotification;

	// Requests
	GetNextMessageRequest *nextMessageRequest;
	RevolverRecieverManager *revolverRecieverManager;
			
	// Events
	void gotStarted();
	void gotStopped();
	void gotMessageNotification(MessageSource newSource);
	void gotTimerTimeout();
	void gotNewMessage();
	void gotNoMessages();
	void gotChannelsChanged();
	
	// State transitions
	void goStartNextMessageRequest();
	void goStopNextMessageRequest();

	void goWaitForTimer();
	
	void goPaused();
	void goResume();

	// Utility functions
	void processReceivedArtMessage(const ArtMessage &message);
	void processReceivedChannelMessage(const ChannelMessage &message, qint32 channelId);

	void changeCurrentSource(MessageSource newSource);

private slots:
	void onTimerTimeout();

	void onGetNotifications(QList <Notification> notifications);

	void onGetNewArtMessage(const ArtMessage &message);
	void onGetNewChannelMessage(const ChannelMessage &message, qint32 channelId);

	void onChannelsChanged();
};

}

#endif // MESSAGERECEIVER_H