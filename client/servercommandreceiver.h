#ifndef SERVERCOMMANDCEIVER_H
#define SERVERCOMMANDCEIVER_H

#include <QQueue>

#include "timerobjects.h"

#include "fault.h"
#include "commandpackage.h"
#include "notification.h"
#include "okresultcode.h"
#include "userstatus.h"

namespace MoodBox
{

// Maximum timeout for retries is 1 min (1 * 60 * 1000 msec)
#define RECEIVER_RETRY_MAXIMUM_INTERVAL				60000

// Receiver timeout for retries is 5 sec
#define RECEIVER_RETRY_INTERVAL						5000

#define DELETE_MESSAGE_COMMAND_TYPE_ID				34

// Class to receive messages
class GetNextCommandsRequest : public RandomRetryTimerObject
{
	Q_OBJECT

public:
	GetNextCommandsRequest(QObject *parent = 0);
	~GetNextCommandsRequest();
	
	void start() { gotStarting(); };
	void stop() { gotStopping(); };
	void setLastPackageId(int lastPackageId) { this->lastPackageId = lastPackageId; };

signals:
	void gotNextCommandPackage(const CommandPackage &commandPackage);

protected:
	/* Request states:
	    NotStarted - does nothing atm
		WaitForServer - wait for server response
		WaitForTimer - wait for retry timer

	  Request workflow:
	   [NotStarted] ->	Send Request ->	[WaitForServer] ->	Got good result -> [NotStarted]
															Got bad result -> [WaitForTimer] -> Send request

	  If request should be stopped it should wait for server response and does not start timer after that
	*/

	enum State { NotStarted, WaitForServer, WaitForTimer };
	State state;

	int lastPackageId;
	
	bool stopRequested;

	// Events
	void gotStarting();
	void gotStopping();
	void gotRequestCompleted();
	void gotRequestResend();
	void gotTimerTimeout();

	// State changes
	void goSend();
	void goStop();
	void goComplete();
	void goResend();
	void goFinish();	

	void sendRequest();
	void complete();

protected slots:
	virtual void onTimerTimeout();

private:
	CommandPackage commandPackage;
	
	void gotRetry();

private slots:
	void onGetNextCommandsResult(QVariant state, Fault fault, CommandPackage result);

};

// Class for receive messages from server
class ServerCommandsReceiver : public QObject
{
	Q_OBJECT

public:
	ServerCommandsReceiver();
	virtual ~ServerCommandsReceiver();

	void start();
	void stop();

private:
	/* Receiver states:
		WaitForNotification - waits for notification about new commands
		WaitForNextCommands - waits for next message request to be completed

	  Receiver workflow:
	   [Paused] -> User online -> [WaitForNextCommands] -> Got commands? -> signal, [WaitForNextCommands]
																		 -> [WaitForNotification]

      Receiver events:
	   1. User is online - unpause and start request for messages
	   2. User is offline - pause
	   3. Got notification about new messages - start request for messages
	   4. Timer timeout - start request for messages
	   5. Got no messages - start timer
	   6. Reload event - do retry for next message request, if started, or go start request for new messages
	   7. Got new message - need to notify about it
	*/

	enum State { WaitForNotification, WaitForNextCommands };
	State state;

	bool paused;

	bool isProcessingNotification;

	// Requests
	GetNextCommandsRequest *nextCommandsRequest;
			
	// Events
	void gotStarted();
	void gotStopped();
	void gotMessageNotification();
	void gotNewCommands();
	void gotNoCommands();
	
	// State transitions
	void goStartNextCommandsRequest();
	void goStopNextCommandsRequest();

	void goPaused();
	void goResume();

	// Utility functions
	void processReceivedCommands(const CommandPackage &commandPackage);

private slots:

	void onGetNotifications(QList <Notification> notifications);

	void onGetNextCommands(const CommandPackage &commandPackage);

};

}

#endif // SERVERCOMMANDCEIVER_H