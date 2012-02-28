#include "messagesender.h"

#include <QFile>
#include <QFileInfo>
#include <QDir>

#include "serverproxysingleton.h"
#include "messagefile.h"
#include "messageorganizer.h"

#include "debug.h"
#include "testtools.h"
#include "faultcodes.h"
#include "faulttools.h"

namespace MoodBox
{

// Comment to hide debug
//#define SHOW_MS_DEBUG					true

#if (defined(UDEBUG)) && (defined(SHOW_MS_DEBUG))
#define MSDEBUG(x)	QDEBUG(x)
#else
#define MSDEBUG(x)
#endif

using namespace Velasquez;

MessageSender::MessageSender(QObject *parent)
	: RandomRetryTimerObject(parent), state(WaitForFiles), paused(true), serverErrorsCount(0)
{
	setTimerIntervalLimit(SENDER_RETRY_MAXIMUM_INTERVAL);
	setRandomTimerInterval(SENDER_RETRY_INTERVAL);

	setRandomTimerRange(0, SENDER_RETRY_INTERVAL);
}

MessageSender::~MessageSender()
{
	if (state != WaitForFiles)
		goStop();
}

void MessageSender::addToQueue(MessageFile *file)
{
	// Start of test code
	/*
	file->setType(MessageType::Private);
	file->setRecipient(-1);
	*/
	// End of test code

	sendQueue.insert(file->getMessageKey(), file);
	
	gotFilesAdded();
}

void MessageSender::addToQueue(const QList<MessageFile *> &files)
{
	if (files.isEmpty())
		return;

	foreach (MessageFile *file, files)
	{
		sendQueue.insert(file->getMessageKey(), file);
	}

	if (!files.isEmpty())
		gotFilesAdded();
}

bool MessageSender::removeFromQueue(const MessageKey &key)
{
	// Do we have one?
	if (!sendQueue.contains(key))
		return true;

	// Cannot do anything with currently processing file
	if (sendQueue.keys().first() == key && !paused)
		return false;

	// Remove from queue and outbox
	MessageFile *file = sendQueue.take(key);
	QString fileName = file->getFileName();

	MessageOrganizer::removeFiles(fileName);

	delete file;

	return true;
}

void MessageSender::init()
{
	gotInit();
}

void  MessageSender::start()
{
	gotStarted();
}

void  MessageSender::stop()
{
	gotStopped();
}

void  MessageSender::cleanup()
{
	gotCleanup();
}

// State operations
void MessageSender::gotInit()
{
	loadOutbox();
}

void MessageSender::gotFilesAdded()
{
	// If we are not in empty state we need to do nothing
	if (state != WaitForFiles)
		return;
	
	goSending();
}

void MessageSender::gotStarted()
{
	MSDEBUG("MessageSender: User is online, resuming sending");

	if (!paused)
		return;

	goContinue();
}

void MessageSender::gotStopped()
{
	MSDEBUG("MessageSender: User is offline, pausing sending");
	
	if (paused)
		return;

	goPaused();
}

void MessageSender::gotTimerTimeout()
{
	if (state == WaitForFiles)
		return;

	goSending();
}

void MessageSender::gotServerResponse()
{
	if (state != WaitForServer)
		return;

	resetAllTries();

	goSending();
}

void MessageSender::gotCleanup()
{
	if (state == WaitForFiles)
		return;

	goStop();
}

void MessageSender::gotNetworkError(Fault fault, const MessageKey &key)
{
	MSDEBUG("MessageSender: gotNetworkError");

	networkError(fault, key);
}

void MessageSender::gotServerError(Fault fault, const MessageKey &key)
{
	MSDEBUG("MessageSender: gotServerError");

	serverError(fault, key);
}

// State transitions
void MessageSender::goSending()
{
	if (sendQueue.isEmpty())
	{
		goWaitForFiles();
		return;
	}

	state = Sending;
	MSDEBUG("MessageSender::state = Sending, waiting for start");
	
	if (!paused)
		goSendFile();
}

void MessageSender::goSendFile()
{
	sendFile(sendQueue.keys().first());
	state = WaitForServer;

	MSDEBUG("MessageSender::state = WaitForServer, waiting for server");
}

void MessageSender::goWaitForFiles()
{
	state = WaitForFiles;

	MSDEBUG("MessageSender::state = WaitForFiles, nothing to do");
}

void MessageSender::goPaused()
{
	paused = true;

	stopTimer();
	resetAllTries();

	MSDEBUG("MessageSender:: paused, waiting for online");
}

void MessageSender::goContinue()
{
	paused = false;

	if (state != WaitForFiles)
		goSending();

	MSDEBUG("MessageSender:: unpaused");
}

void MessageSender::goStop()
{
	MSDEBUG("MessageSender::stopping");

	goPaused();

	cancelQueue();
}

// Send operations
void MessageSender::loadOutbox()
{
	QDir outbox(MessageOrganizer::getOutboxFolder());

	QList <MessageFile *> files;

	QStringList filter;
	filter.append("*" + tr(MESSAGE_FILE_EXTENSION));
	QFileInfoList fileList = outbox.entryInfoList(filter, QDir::Files, QDir::Name);

	foreach (QFileInfo fileInfo, fileList)
	{
		MessageFile *file = new MessageFile();
		file->setFileName(fileInfo.absoluteFilePath());

		XmlSerializable::SerializationResult result = file->load();

		QString previewFileName = MessageOrganizer::getPreviewFileName(fileInfo.absoluteFilePath());
		QImage preview(previewFileName);
		file->setPreview(preview);

		if (result == XmlSerializable::Ok && !preview.isNull())
			files.append(file);
		else
			delete file;
	}

	addToQueue(files);

	MSDEBUG("MessageSender::Message outbox loaded - " << files.count());
}

void MessageSender::sendFile(const MessageKey &key)
{
	MessageFile *file = sendQueue[key];
	
	switch (file->getMessageType())
	{
		case MessageType::Private:
			SERVER->sendPrivateMessage(CALLBACK(this, onSendMessageResult, SendMessageResult), key.toVariant(), file->getRecipient(), file->getPreviewBytes(), file->getInfo());
			break;
	
		case MessageType::Friends:
			SERVER->sendFriendMessage(CALLBACK(this, onSendMessageResult, SendMessageResult), key.toVariant(), file->getPublic(), file->getPreviewBytes(), file->getInfo());
			break;

		case MessageType::Channel:
			SERVER->sendChannelMessage(CALLBACK(this, onSendMessageResult, SendMessageResult), key.toVariant(), file->getRecipient(), file->getPreviewBytes(), file->getInfo());
			break;

		default:
			Q_ASSERT_X(false, "MessageSender::sendFile", "Not supported file type");
	}
}

void MessageSender::cancelQueue()
{
	while (sendQueue.count() > 0)
	{
		MessageKey key = sendQueue.keys().last();

		MessageFile *file = sendQueue.take(key);
		delete file;
	}
}

void MessageSender::resetAllTries()
{
	clearTryNumber();
	serverErrorsCount = 0;
}

void MessageSender::fileSent(const MessageKey &key, const SendMessageResult &result)
{
	MessageFile *file = sendQueue.take(key);

	// Occurs when object deletes
	if (file == NULL)
		return;

	QString outboxFileName = file->getFileName();
	QString outboxPreviewName = MessageOrganizer::getPreviewFileName(outboxFileName);

	// Update information from response
	if (result.getResultCode() == ContactResultCode::Ok || 
		result.getResultCode() == ContactResultCode::NotAuthorizedMe ||
		result.getResultCode() == ContactResultCode::ClosedContact)
	{
		file->setId(result.getMessageId());
		file->setSentDate(result.getSendDate());
		file->setSent(true);

		updateMessageFile(file, key);

		// Notify about ban
		if (result.getResultCode() == ContactResultCode::NotAuthorizedMe || 
			result.getResultCode() == ContactResultCode::ClosedContact)
		{
			emit showExceptionDialogSendingMessage(result.getResultCode(), file->getRecipient());
		}

		MSDEBUG("MessageSender::Message was sent " << key.getDate());
	}
	
	else
	{
		MSDEBUG("MessageSender::Message was not sent " << key << " code " << result.getResultCode());
	}

	// Notify
	sendSentNotification(file, result.getResultCode(), key);

	// Delete from outbox and queue
	QFile::remove(outboxFileName);
	QFile::remove(outboxPreviewName);

	delete file;
}

void MessageSender::rejectFile(Fault fault, const MessageKey &key)
{
	MessageFile *file = sendQueue.take(key);

	// Occurs when object deletes
	if (file == NULL)
		return;
	
	QString outboxFileName = file->getFileName();
	QString outboxPreviewName = MessageOrganizer::getPreviewFileName(outboxFileName);

	// Save to Rejected with info
	QString rejectedName = MessageOrganizer::getRejectedFileName(MessageOrganizer::getFileName(key));
	QFile::copy(outboxFileName, rejectedName);

	QString rejectedPreviewName = MessageOrganizer::getPreviewFileName(rejectedName);
	QFile::copy(outboxPreviewName, rejectedPreviewName);
	
	QString faultName = rejectedName.replace(MESSAGE_FILE_EXTENSION, FAULT_FILE_EXTENSION);
	FaultTools::saveFault(&fault, faultName);

	// Delete from outbox and queue
	QFile::remove(outboxFileName);
	QFile::remove(outboxPreviewName);
	delete file;
}

void MessageSender::updateMessageFile(MessageFile *file, const MessageKey &oldKey)
{
	QString oldFileName = MessageOrganizer::getFileName(oldKey);
	QString newFileName = MessageOrganizer::getFileName(file->getMessageKey());

	switch (file->getMessageType())
	{
		case MessageType::Channel:
		case MessageType::Private:
			oldFileName = MessageOrganizer::getPrivateMessageFileName(file->getRecipient(), oldFileName);
			newFileName = MessageOrganizer::getPrivateMessageFileName(file->getRecipient(), newFileName);
			break;

		case MessageType::Friends:
			oldFileName = MessageOrganizer::getFriendsMessageFileName(oldFileName);
			newFileName = MessageOrganizer::getFriendsMessageFileName(newFileName);
			break;

		default:
			Q_ASSERT_X(false, "MessageSender::updateMessageFile", "Not supported file type");
	}

	file->setFileName(newFileName);
	file->save();

	MSDEBUG("MessageSender::Message updated " << newFileName);

	// Remove old file and update preview file
	QFile::remove(oldFileName);
	QString oldPreviewName = MessageOrganizer::getPreviewFileName(oldFileName);
	QString newPreviewName = MessageOrganizer::getPreviewFileName(newFileName);
	QFile::rename(oldPreviewName, newPreviewName);

	// Update thumbnail
	QFile::rename(MessageOrganizer::getThumbnailFileName(oldPreviewName), MessageOrganizer::getThumbnailFileName(newPreviewName));
}

void MessageSender::sendSentNotification(MessageFile *file, ContactResultCode::ContactResultCodeEnum result, const MessageKey &oldKey)
{
	switch (file->getMessageType())
	{
		case MessageType::Channel:
		case MessageType::Private:
			emit privateMessageSent(file->getRecipient(), file->getMessageKey(), result, oldKey);
			break;

		case MessageType::Friends:
			emit friendsMessageSent(file->getMessageKey(), result, oldKey);
			break;
		
		default:
			Q_ASSERT_X(false, "MessageSender::sendSentNotification", "Not supported file type");
	}
}

void MessageSender::networkError(Fault fault, const MessageKey &key)
{
	if (serverErrorsCount >= NETWORK_REJECTED_RETRIES)
	{
		MSDEBUG("MessageSender: Network rejecting file " << key.getDate());
		
		rejectFile(fault, key);
		serverErrorsCount = 0;

		emit sendError();

		gotServerResponse();
	}
	else
	{
		MSDEBUG("MessageSender: Network re-trying with interval " << getRetryTimerInterval());
		
		if (serverErrorsCount == 0)
			clearTryNumber();
		
		serverErrorsCount ++;
		startNewTryTimer();
	}
}

void MessageSender::serverError(Fault fault, const MessageKey &key)
{
	if (fault.getCode() == FAULT_REQUEST_TOO_LARGE || serverErrorsCount >= SENDER_REJECTED_RETRIES)
	{
		MSDEBUG("MessageSender: Rejecting file " << key.getDate());
		
		rejectFile(fault, key);
		serverErrorsCount = 0;

		emit sendError();

		gotServerResponse();
	}
	else
	{
		MSDEBUG("MessageSender: Re-trying with interval " << getRetryTimerInterval());
		
		if (serverErrorsCount == 0)
			clearTryNumber();
		
		serverErrorsCount ++;
		startNewTryTimer();
	}
}

void MessageSender::onTimerTimeout()
{
	gotTimerTimeout();
}

void MessageSender::onSendMessageResult(QVariant state, Fault fault, SendMessageResult result)
{
	MessageKey key = state.value<MessageKey>();

	// Start of test code
	/*
	static TestCounter counter(2);
	
	if (counter.needMore())
		fault = Fault(FAULT_REQUEST_TOO_LARGE, FAULT_REQUEST_TOO_LARGE, FAULT_REQUEST_TOO_LARGE);
	
	static TestCounter counter2(5);
	if (counter.getNow() >= counter.getCount() )
		if (counter2.needMore())
			fault = Fault(FAULT_TRANSPORT_ERROR, "test", "test");
	
	static TestCounter counter3(4);
	
	if (counter3.needMore())
		fault = Fault(FAULT_TRANSPORT_ERROR, "test", "test");*/

	// End of test code

	if (!fault.isNull())
	{
		if (FaultTools::isNetworkError(fault))
			gotNetworkError(fault, key);
		else
			gotServerError(fault, key);
	}
	else
	{		
		fileSent(key, result);

		gotServerResponse();
	}	
}

}
