#include "servercommandreceiver.h"

#include "serverproxysingleton.h"
#include "mtrandom.h"
#include "peopleinfomanager.h"
#include "packageunion.h"
#include "messagemanager.h"

#include "debug.h"
#include "testtools.h"

// Comment to hide debug
//#define SHOW_MRDEBUG					true

#if (defined(UDEBUG)) && (defined(SHOW_MRDEBUG))
#define MRDEBUG(x)	QDEBUG(x)
#else
#define MRDEBUG(x)
#endif

namespace MoodBox
{

MTRandom _commandReceiverRandom;

// GetNextMessageRequest class
GetNextCommandsRequest::GetNextCommandsRequest(QObject *parent)
	: RandomRetryTimerObject(parent), state(NotStarted), stopRequested(false)
{
	setTimerIntervalLimit(RECEIVER_RETRY_MAXIMUM_INTERVAL);
	setRandomTimerInterval(RECEIVER_RETRY_INTERVAL);
	setRandomTimerRange(0, RECEIVER_RETRY_INTERVAL);

	lastPackageId = 0;
}

GetNextCommandsRequest::~GetNextCommandsRequest()
{
	stop();
}

void GetNextCommandsRequest::gotStarting()
{
	if (state != NotStarted)
		return;

	clearTryNumber();

	goSend();
}

void GetNextCommandsRequest::gotStopping()
{
	if (state == NotStarted || stopRequested)
		return;

	stopRequested = true;
	goStop();
}

void GetNextCommandsRequest::gotRequestCompleted()
{
	if (state != WaitForServer)
		return;

	goComplete();
}

void GetNextCommandsRequest::gotRequestResend()
{
	if (state != WaitForServer)
		return;

	if (stopRequested)
		goFinish();
	else
		goResend();
}

void GetNextCommandsRequest::gotTimerTimeout()
{
	if (state != WaitForTimer)
		return;

	goSend();
}

// State change
void GetNextCommandsRequest::goSend()
{
	state = WaitForServer;

	sendRequest();
}

void GetNextCommandsRequest::goStop()
{
	// If we do not wait for timer process nor for http the process will be stopped in other way
	switch (state)
	{
		case WaitForTimer:
			stopTimer();
			goFinish();

			break;
	}
}

void GetNextCommandsRequest::goComplete()
{
	goFinish();

	complete();
}

void GetNextCommandsRequest::goResend()
{
	if (!hasMoreTries())
	{
		MRDEBUG("CommandsReceiverRequest: No more tries, giving up at " << getRetryTimerInterval() << " try number " << getTryNumber());
	    
		goComplete();
	}
	else
	{
		MRDEBUG("CommandsReceiverRequest: Resend re-try with interval " << getRetryTimerInterval());
		
		startNewTryTimer();
		state = WaitForTimer;
	}
}

void GetNextCommandsRequest::goFinish()
{
	stopRequested = false;
	state = NotStarted;
}

void GetNextCommandsRequest::onTimerTimeout()
{
	gotTimerTimeout();
}

void GetNextCommandsRequest::sendRequest()
{
	MRDEBUG("GetNextMessageRequest: Request for next message sent");
	SERVER->getCommands(CALLBACK(this, onGetNextCommandsResult, CommandPackage), QVariant(), lastPackageId);
}

void GetNextCommandsRequest::complete()
{
	emit gotNextCommandPackage(commandPackage);
}

void GetNextCommandsRequest::gotRetry()
{
	clearTryNumber();

	gotRequestResend();
}

void GetNextCommandsRequest::onGetNextCommandsResult(QVariant state, Fault fault, CommandPackage result)
{
	Q_UNUSED(state)
	MRDEBUG("GetNextCommandsRequest: Request for next commands received");

	if (!fault.isNull())
	{
		MRDEBUG("GetNextCommandsRequest: Got problem with next commands request");
		gotRequestResend();
	}
	else
	{
		// If there is no message we're done
		commandPackage = result;

		if (result.isNull())
		{
			// No new messages
			lastPackageId = 0;
			gotRequestCompleted();
			return;
		}

		gotRequestCompleted();
	}
}

// ServerCommandsReceiver class
ServerCommandsReceiver::ServerCommandsReceiver()
: state(WaitForNotification), paused(true), isProcessingNotification(false)
{
	// Requests
	nextCommandsRequest = new GetNextCommandsRequest(this);

	connect(nextCommandsRequest, SIGNAL(gotNextCommandPackage(const CommandPackage &)), this, SLOT(onGetNextCommands(const CommandPackage &)));

	// Server and manager connections for updates
	connect(SERVER, SIGNAL(notificationResult(QList<Notification>)), this, SLOT(onGetNotifications(QList<Notification>)));
}

ServerCommandsReceiver::~ServerCommandsReceiver()
{
	// Pause requests
	gotStopped();
}

void ServerCommandsReceiver::start()
{
	gotStarted();
}

void ServerCommandsReceiver::stop()
{
	gotStopped();
}

void ServerCommandsReceiver::gotStarted()
{
	MRDEBUG("ServerCommandsReceiver: User is online, resuming receiving");

	if (!paused)
		return;

	goResume();
}

void ServerCommandsReceiver::gotStopped()
{
	MRDEBUG("ServerCommandsReceiver: User is offline, pausing receiving");
	
	if (paused)
		return;

	goPaused();
}

void ServerCommandsReceiver::gotMessageNotification()
{
	if (state != WaitForNotification)
	{
		return;
	}

	isProcessingNotification = true;

	state = WaitForNotification;

	goStartNextCommandsRequest();
}

void ServerCommandsReceiver::gotNewCommands()
{
	if (state != WaitForNextCommands)
		return;

	goStartNextCommandsRequest();
}

void ServerCommandsReceiver::gotNoCommands()
{
	if (state != WaitForNextCommands)
		return;

	isProcessingNotification = false;

	state = WaitForNotification;
}

// State changes
void ServerCommandsReceiver::goStartNextCommandsRequest()
{
	MRDEBUG("ServerCommandsReceiver: Next commands request started");

	nextCommandsRequest->start();

	state = WaitForNextCommands;
}

void ServerCommandsReceiver::goStopNextCommandsRequest()
{
	MRDEBUG("ServerCommandsReceiver: Commands request stopped");
	nextCommandsRequest->stop();
}

void ServerCommandsReceiver::goPaused()
{
	MRDEBUG("ServerCommandsReceiver: Pausing commands receive, state = " << state);

	paused = true;

	isProcessingNotification = false;

	if (state == WaitForNextCommands)
		goStopNextCommandsRequest();
}

void ServerCommandsReceiver::goResume()
{
	MRDEBUG("ServerCommandsReceiver: Resuming message receive, state = " << state);

	paused = false;
	
	isProcessingNotification = false;

	goStartNextCommandsRequest();
}

void ServerCommandsReceiver::processReceivedCommands(const CommandPackage &commandPackage)
{
	// Make sure we are in a right place
	if (!INFOMANAGER->getIsLoggedOn())
		return;

	bool empty = commandPackage.isNull();

	nextCommandsRequest->setLastPackageId(commandPackage.getPackageId());
	
	if (!empty)
	{
		foreach (PackageUnion commandIterator, commandPackage.getItems())
		{
			switch (commandIterator.getValue()->getTypeId())
			{
				case DELETE_MESSAGE_COMMAND_TYPE_ID:
					DeleteMessageCommand *deleteMessageCommand = dynamic_cast<DeleteMessageCommand*>(commandIterator.getValue());
					MESSAGEMANAGER->deleteMessage(deleteMessageCommand->getMessageId(), MessageType::Channel, deleteMessageCommand->getContactId());
					break;
			}
		}
	}
}

void ServerCommandsReceiver::onGetNotifications(QList <Notification> notifications)
{
	foreach (Notification notification, notifications)
	{
		switch (notification.getEvent())
		{
			case Event::NewCommand:
				MRDEBUG("ServerCommandsReceiver: Got new command notification");

				gotMessageNotification();
				break;
		}
	}
}

void ServerCommandsReceiver::onGetNextCommands(const CommandPackage &commandPackage)
{
	if (commandPackage.isNull())
	{
		MRDEBUG("ServerCommandsReceiver: No new commands");
		gotNoCommands();
	}
	else
	{
		MRDEBUG("ServerCommandsReceiver: Got new commands");
		processReceivedCommands(commandPackage);
		gotNewCommands();
	}
}

}
