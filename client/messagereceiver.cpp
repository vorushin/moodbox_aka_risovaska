#include "messagereceiver.h"

#include <QNetworkRequest>
#include <QNetworkReply>

#include "serverproxysingleton.h"
#include "peopleinfomanager.h"
#include "messagefile.h"
#include "mtrandom.h"
#include "channelinfomanager.h"
#include "httptools.h"
#include "metainfoprovider.h"

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

MTRandom _receiverRandom;

// Class to manage receiving messages (Art and Channel messages)
RevolverRecieverManager::RevolverRecieverManager(QObject *parent)
	: SingleShotTimerObject(parent)
{
	currentSourceIndex = -1;
	setTimerInterval(RECEIVER_CHANNELS_UPDATE_INTERVAL);

	connect(INFOMANAGER, SIGNAL(contactListChanged()), this, SLOT(onContactListChanged()));
}

void RevolverRecieverManager::start()
{
	resetIndex();
	requestQueue.clear();	
	stopTimer();
}

void RevolverRecieverManager::putSourceInQueue(MessageSource source)
{
	if (!requestQueue.contains(source))
		requestQueue.enqueue(source);
}

void RevolverRecieverManager::onTimerTimeout()
{
	emit channelsChanged();
}

MessageSource RevolverRecieverManager::getNextMessageSource()
{
	if (!requestQueue.isEmpty())
	{
		return requestQueue.dequeue();
	}
	else
		if (currentSourceIndex < 0 || currentSourceIndex >= sources.size())
		{	
			return MessageSource();
		}
		else
		{
			return sources[currentSourceIndex++];
		}	
}

MessageSource RevolverRecieverManager::getNextMessageSourceCycled()
{
	MessageSource source = getNextMessageSource();

	if (source.mode == SourceUndefined)
	{
		resetIndex();

		source = getNextMessageSource();
	}

	return source;
}

void RevolverRecieverManager::onContactListChanged()
{
	stopTimer();

	MessageSource currentSource;
	if (currentSourceIndex < 0 || currentSourceIndex >= sources.size())
	{	
		currentSource = MessageSource();
	}
	else
	{
		currentSource = sources[currentSourceIndex];
	}

	QList <qint32> oldKeys = sources.keys();
	sources.clear();

	// by default first source of messages is Artmessages
	int index = 0;

	MessageSource source(ArtMessages, -1);
	sources.insert(index, source);

	source.mode = ChannelMessages;
	foreach(ContactInfo* infoContact, INFOMANAGER->getContacts())
	{
		if (infoContact->getType() == ContactType::Channel)
		{
			source.channelId = infoContact->getUserId();
			sources.insert(++index, source);
		}
	}

	if (currentSource.mode != SourceUndefined)
	{
		if (currentSourceIndex < 0 || currentSourceIndex >= sources.size())
		{	
			currentSourceIndex = 0;
		}
		else
			if (sources[currentSourceIndex].mode != currentSource.mode || sources[currentSourceIndex].channelId != currentSource.channelId )
			{
			   currentSourceIndex = 0;
			}
	}

	// Check for new channels
	bool shouldNotify = false;

	foreach (qint32 sourceId, sources.keys())
	{
		if (!oldKeys.contains(sourceId))
		{
			shouldNotify = true;
			break;
		}
	}

	if (shouldNotify)
		startTimer();
}

void RevolverRecieverManager::resetIndex()
{
	currentSourceIndex = sources.isEmpty() ? -1 : 0;
}

// GetNextMessageRequest class
GetNextMessageRequest::GetNextMessageRequest(QObject *parent)
	: RandomRetryTimerObject(parent), state(NotStarted), stopRequested(false), skipMessage(false)
{
	setTimerIntervalLimit(RECEIVER_RETRY_MAXIMUM_INTERVAL);
	setRandomTimerInterval(RECEIVER_RETRY_INTERVAL);
	setRandomTimerRange(0, RECEIVER_RETRY_INTERVAL);

	source.channelId = -1;
	source.mode = ArtMessages;

	lastMessageId = 0;

	http = new HttpLoader(this);
	connect(http, SIGNAL(finished(QNetworkReply *)), this, SLOT(onGetNetworkReply(QNetworkReply *)));
}

GetNextMessageRequest::~GetNextMessageRequest()
{
	stop();
}

void GetNextMessageRequest::gotStarting()
{
	if (state != NotStarted)
		return;

	clearTryNumber();

	goSend();
}

void GetNextMessageRequest::gotStopping()
{
	if (state == NotStarted || stopRequested)
		return;

	stopRequested = true;
	goStop();
}

void GetNextMessageRequest::gotRequestCompleted()
{
	if (state != WaitForServer && state != WaitForHttp)
		return;

	goComplete();
}

void GetNextMessageRequest::gotRequestResend()
{
	if (state != WaitForServer)
		return;

	if (stopRequested)
		goFinish();
	else
		goResend();
}

void GetNextMessageRequest::gotUrl()
{
	if (state != WaitForServer)
		return;

	goLoadUrl();
}

void GetNextMessageRequest::gotTimerTimeout()
{
	if (state != WaitForTimer)
		return;

	goSend();
}

// State change
void GetNextMessageRequest::goSend()
{
	state = WaitForServer;

	sendRequest();
}

void GetNextMessageRequest::goStop()
{
	// If we do not wait for timer process nor for http the process will be stopped in other way
	switch (state)
	{
		case WaitForHttp:
			http->stop();
			goFinish();

			break;
	
		case WaitForTimer:
			stopTimer();
			goFinish();

			break;
	}
}

void GetNextMessageRequest::goComplete()
{
	skipMessage = false;

	goFinish();

	complete();
}

void GetNextMessageRequest::goResend()
{
	skipMessage = false;

	if (!hasMoreTries())
	{
		MRDEBUG("MessageReceiverRequest: No more tries, giving up at " << getRetryTimerInterval() << " try number " << getTryNumber());
	    
		if (source.mode == ChannelMessages)
		{
			skipMessage = true;
			
			goFinish();

			gotStarting();
		}
		else
		{
			goComplete();
		}
	}
	else
	{
		MRDEBUG("MessageReceiverRequest: Resend re-try with interval " << getRetryTimerInterval());
		
		startNewTryTimer();
		state = WaitForTimer;
	}
}

void GetNextMessageRequest::goLoadUrl()
{
	state = WaitForHttp;

	loadUrl();
}

void GetNextMessageRequest::goFinish()
{
	stopRequested = false;
	state = NotStarted;
}

void GetNextMessageRequest::onTimerTimeout()
{
	gotTimerTimeout();
}

void GetNextMessageRequest::sendRequest()
{
	MRDEBUG("GetNextMessageRequest: Request for next message sent");
	switch (source.mode)
	{
		case ArtMessages:
			SERVER->getNextArtmessageAndReport(CALLBACK(this, onGetNextArtMessageResult, ArtMessage), QVariant(), lastMessageId);
			break;

		case ChannelMessages:
			lastMessageId = CHANNELMANAGER->getLastChannelMessage(source.channelId);
			SERVER->getNextChannelMessageUrl(CALLBACK(this, onGetNextChannelMessageUrlResult, ChannelMessageUrl), QVariant(), source.channelId, lastMessageId, skipMessage);
			break;
	}
}

void GetNextMessageRequest::loadUrl()
{
	MRDEBUG("GetNextMessageRequest: Sending request for URL data");

	QNetworkRequest request(url);
	http->get(request);
}

void GetNextMessageRequest::complete()
{
	switch (source.mode)
	{
		case ArtMessages:
			emit gotNextArtMessage(artMessage);
			break;

		case ChannelMessages:
			emit gotNextChannelMessage(channelMessage, source.channelId);
			break;
	}
}

void GetNextMessageRequest::gotRetry()
{
	clearTryNumber();

	gotRequestResend();
}

void GetNextMessageRequest::onGetNextArtMessageResult(QVariant state, Fault fault, ArtMessage result)
{
	Q_UNUSED(state)
	MRDEBUG("GetNextMessageRequest: Request for next message received");

	// Start of test code
	/*
	static TestCounter counter(5);
	
	if (counter.needMore())
		fault = Fault("test", "test", "test");
	*/
	// End of test code

	if (!fault.isNull())
	{
		MRDEBUG("GetNextMessageRequest: Got problem with next message request");
		gotRequestResend();
	}
	else
	{
		// If there is no message we're done
		artMessage = result;

		if (result.isNull())
		{
			// No new messages
			lastMessageId = 0;
			gotRequestCompleted();
			return;
		}

		// Check for URL and download it if needed
		if (!result.getUrl().isEmpty())
		{
			url = result.getUrl();

			lastMessageId = result.getMessageId();

			gotUrl();
			return;
		}

		// Start test code
		/*
		static TestCounter counter2(5);

		if (counter2.needMore())
		{
			result.setResultCode(ArtmessageResultCode::MessageDataNotFound);			
		}
		*/
		// End test code

		// If we got empty data we need to retry
		if (result.getResultCode() == ArtmessageResultCode::MessageDataNotFound)
		{
			MRDEBUG("GetNextMessageRequest: Got empty message data, retrying");
			
			// Make sure there is really no data
			artMessage.setData(QByteArray());
			
			gotRequestResend();
			return;
		}

		gotRequestCompleted();
	}
}

void GetNextMessageRequest::onGetNextChannelMessageUrlResult(QVariant state, Fault fault, ChannelMessageUrl result)
{
	Q_UNUSED(state)
	MRDEBUG("GetNextMessageRequest: Request for next channel message received");

	if (!fault.isNull())
	{
		MRDEBUG("GetNextMessageRequest: Got problem with next message request");
		gotRequestResend();
	}
	else
	{
		channelMessage = ChannelMessage();

		// If there is no message we're done
		if (result.isNull())
		{
			// No new messages
			gotRequestCompleted();
			return;
		}

		// Filling in all except actual data
		// TODO: add actual metadata
		channelMessage = ChannelMessage(result.getResultCode(), result.getMessageId(), result.getAuthorId(), result.getAuthorLogin(), result.getSendDate(), QByteArray(), QByteArray());
		url = result.getUrl();

		CHANNELMANAGER->setLastChannelMessage(source.channelId, result.getMessageId());

		gotUrl();
	}
}

void GetNextMessageRequest::onGetNetworkReply(QNetworkReply *reply)
{
	MRDEBUG("GetNextMessageRequest: Got reply on HTTP request");

	if (reply != NULL)
	{
		QString imageType = reply->header(QNetworkRequest::ContentTypeHeader).toString();
		QString info = Velasquez::MetaInfoProvider::getTitleString(METAINFO_IMAGEFORMAT_TITLE, Velasquez::MetaInfoProvider::getTagString(METAINFO_IMAGEFORMAT_TAGNAME, imageType));

		// TODO: put correct metadata
		switch (source.mode)
		{
			case ArtMessages:
				artMessage.setData(reply->readAll());
				artMessage.setMetadata(info);
				break;

			case ChannelMessages:
				channelMessage.setData(reply->readAll());
				channelMessage.setMetadata(info);
				break;
		}

		gotRequestCompleted();
	}
	else
	{
		goFinish();

		gotStarting();
	}	
}

// MessageReceiver class
MessageReceiver::MessageReceiver(QObject *parent)
	: RandomRetryTimerObject(parent), state(WaitForTimer), 
	  hadChannelMessages(false), channelMessagesCount(0), paused(true), isProcessingNotification(false)
{
	setRandomTimerInterval(RECEIVER_PERIODIC_UPDATE_INTERVAL);	
	setRandomTimerRange(1000, 2000);

	changeCurrentSource(MessageSource(ArtMessages, -1));

	// Requests
	nextMessageRequest = new GetNextMessageRequest(this);
	revolverRecieverManager = new RevolverRecieverManager(this);

	connect(nextMessageRequest, SIGNAL(gotNextArtMessage(const ArtMessage &)), this, SLOT(onGetNewArtMessage(const ArtMessage &)));
	connect(nextMessageRequest, SIGNAL(gotNextChannelMessage(const ChannelMessage &, qint32)), this, SLOT(onGetNewChannelMessage(const ChannelMessage &, qint32)));

	connect(revolverRecieverManager, SIGNAL(channelsChanged()), this, SLOT(onChannelsChanged()));

	// Server and manager connections for updates
	connect(SERVER, SIGNAL(notificationResult(QList<Notification>)), this, SLOT(onGetNotifications(QList<Notification>)));
}

MessageReceiver::~MessageReceiver()
{
	// Pause requests
	gotStopped();
}

void MessageReceiver::start()
{
	gotStarted();
}

void MessageReceiver::stop()
{
	gotStopped();
}

void MessageReceiver::gotStarted()
{
	MRDEBUG("MessageReceiver: User is online, resuming receiving");

	if (!paused)
		return;

	goResume();
}

void MessageReceiver::gotStopped()
{
	MRDEBUG("MessageReceiver: User is offline, pausing receiving");
	
	if (paused)
		return;

	goPaused();
}

void MessageReceiver::gotMessageNotification(MessageSource newSource)
{
	if (state != WaitForTimer)
	{
		if (newSource.mode != currentSource.mode && 
			(newSource.mode == ArtMessages || 
			 (newSource.mode == ChannelMessages && newSource.channelId != currentSource.channelId)))

			revolverRecieverManager->putSourceInQueue(newSource);

		return;
	}

	isProcessingNotification = true;

	changeCurrentSource(newSource);

	stopTimer();

	goStartNextMessageRequest();
}

void MessageReceiver::gotTimerTimeout()
{
	if (state != WaitForTimer || paused)
		goWaitForTimer();
	else
		goResume();
}

void MessageReceiver::gotNewMessage()
{
	if (state != WaitForNextMessage)
		return;

	if (currentSource.mode == ChannelMessages)
	{
		MRDEBUG("MessageReceiver: got channel message, channelMessagesCount " << channelMessagesCount);

		// If there are too much messages in channel
		if (++channelMessagesCount >= RECEIVER_MAXIMUM_CHANNEL_MESSAGES_SESSION)
		{
			hadChannelMessages = true;
			isProcessingNotification = false;

			// Switch the source and reset the counter
			changeCurrentSource(revolverRecieverManager->getNextMessageSourceCycled());
		}
	}

	goStartNextMessageRequest();
}

void MessageReceiver::gotNoMessages()
{
	if (state != WaitForNextMessage)
		return;

	MessageSource newSource = revolverRecieverManager->getNextMessageSource();
	
	if (newSource.mode == SourceUndefined)
	{
		if (hadChannelMessages && !isProcessingNotification)
		{			
			MRDEBUG("MessageReceiver: restarting revolver to check for messages again");
			hadChannelMessages = false;

			newSource = revolverRecieverManager->getNextMessageSourceCycled();			
		}
		else
		{
			isProcessingNotification = false;

			MRDEBUG("MessageReceiver: No message sources left");
			goWaitForTimer();

			return;
		}
	}

	isProcessingNotification = false;

	changeCurrentSource(newSource);
	goStartNextMessageRequest();
}

void MessageReceiver::gotChannelsChanged()
{
	if (state != WaitForTimer || paused)
		return;

	// Restarting timer
	stopTimer();
	gotTimerTimeout();
}

// State changes
void MessageReceiver::goStartNextMessageRequest()
{
	MRDEBUG("MessageReceiver: Next message request started");

	nextMessageRequest->setSource(currentSource);
	nextMessageRequest->start();

	state = WaitForNextMessage;
}

void MessageReceiver::goStopNextMessageRequest()
{
	MRDEBUG("MessageReceiver: Message request stopped");
	nextMessageRequest->stop();
}

void MessageReceiver::goWaitForTimer()
{
	MRDEBUG("MessageReceiver: Wait timer started");

	// No need to accumulate tries
	clearTryNumber();
	startNewTryTimer();

	state = WaitForTimer;
}

void MessageReceiver::goPaused()
{
	MRDEBUG("MessageReceiver: Pausing message receive, state = " << state);

	paused = true;

	isProcessingNotification = false;

	if (state == WaitForTimer)
		stopTimer();

	if (state == WaitForNextMessage)
		goStopNextMessageRequest();
}

void MessageReceiver::goResume()
{
	MRDEBUG("MessageReceiver: Resuming message receive, state = " << state);

	paused = false;
	
	hadChannelMessages = false;

	isProcessingNotification = false;

	revolverRecieverManager->start();

	changeCurrentSource(revolverRecieverManager->getNextMessageSource());

	goStartNextMessageRequest();
}

void MessageReceiver::processReceivedArtMessage(const ArtMessage &message)
{
	// Make sure we are in a right place
	if (!INFOMANAGER->getIsLoggedOn())
		return;

	bool empty = message.getData().isEmpty();
	MessageFile *messageFile = NULL;

	if (!empty)
	{
		messageFile = new MessageFile();

		messageFile->setRecipient(message.getType(), INFOMANAGER->getUserAccount().getId());
		messageFile->setAuthor(message.getAuthorId());
		messageFile->setSentDate(message.getSendDate());
		messageFile->setId(message.getMessageId());
		
		messageFile->setInfo(message.getMetadata());
		messageFile->setPreview(message.getData());

		messageFile->setSent(true);
	}

	nextMessageRequest->setLastMessageId(message.getMessageId());
	
	if (!empty)
		emit messageReceived(messageFile);
}

void MessageReceiver::processReceivedChannelMessage(const ChannelMessage &message, qint32 channelId)
{
	// Make sure we are in a right place
	if (!INFOMANAGER->getIsLoggedOn())
		return;

	bool empty = message.getData().isEmpty();
	MessageFile *messageFile = NULL;

	if (!empty)
	{
		messageFile = new MessageFile();
		
		messageFile->setRecipient(MessageType::Channel, channelId);
		messageFile->setAuthor(message.getAuthorId());
		messageFile->setAuthorLogin(message.getAuthorLogin());
		messageFile->setSentDate(message.getSendDate());
		messageFile->setId(message.getMessageId());

		messageFile->setInfo(message.getMetadata());
		messageFile->setPreview(message.getData());

		messageFile->setSent(true);
	}

	if (!empty)
		emit messageReceived(messageFile);
}

void MessageReceiver::changeCurrentSource(MessageSource newSource)
{
	MRDEBUG("MessageReceiver: changing message source " << newSource.channelId);

	currentSource = newSource;
	channelMessagesCount = 0;
}

void MessageReceiver::onTimerTimeout()
{
	MRDEBUG("MessageReceiver: Timer timeout");

	gotTimerTimeout();
}

void MessageReceiver::onGetNotifications(QList <Notification> notifications)
{
	MessageSource newSource;

	foreach (Notification notification, notifications)
	{
		switch (notification.getEvent())
		{
			case Event::NewMessage:
				MRDEBUG("MessageReceiver: Got new message notification");

				newSource.mode = ArtMessages;
				newSource.channelId = -1;

				gotMessageNotification(newSource);
				break;

			case Event::NewChannelMessage:
				MRDEBUG("MessageReceiver: Got new channel message notification");

				newSource.mode = ChannelMessages;
				newSource.channelId = notification.getUserId();
				
				gotMessageNotification(newSource);
				break;
		}
	}
}

void MessageReceiver::onGetNewArtMessage(const ArtMessage &message)
{
	if (message.isNull())
	{
		MRDEBUG("MessageReceiver: No new messages");
		gotNoMessages();
	}
	else
	{
		MRDEBUG("MessageReceiver: Got new messages");
		processReceivedArtMessage(message);
		gotNewMessage();
	}
}

void MessageReceiver::onGetNewChannelMessage(const ChannelMessage &message, qint32 channelId)
{
	if (message.isNull())
	{
		MRDEBUG("MessageReceiver: No new messages");
		gotNoMessages();
	}
	else
	{
		MRDEBUG("MessageReceiver: Got new messages");
		processReceivedChannelMessage(message, channelId);
		gotNewMessage();
	}
}

void MessageReceiver::onChannelsChanged()
{
	gotChannelsChanged();
}

}
