#include "messagepublisher.h"

#include <QFile>
#include <QFileInfo>
#include <QByteArray>
#include <QBuffer>

#include "serverproxysingleton.h"
#include "peopleinfomanager.h"
#include "imagetools.h"
#include "imageelement.h"
#include "messagefile.h"

#include "debug.h"
#include "testtools.h"
#include "faulttools.h"
#include "common.h"

using namespace Velasquez;

namespace MoodBox
{

// Comment to hide debug
//#define SHOW_MP_DEBUG					true

#if (defined(UDEBUG)) && (defined(SHOW_MP_DEBUG))
#define MPDEBUG(x)	QDEBUG(x)
#else
#define MPDEBUG(x)
#endif

#define MESSAGE_INFO_SIZE		0
#define MESSAGE_INFO_PERCENT	1

// MoodstripPublisher class
MoodstripPublisher::MoodstripPublisher(QObject *parent)
	: RandomRetryTimerObject(parent), state(WaitForMoodstrip), paused(true), id(-1), recipientId(-1), isHidden(false)
{
	setTimerIntervalLimit(PUBLISH_RETRY_MAXIMUM_INTERVAL);
	setRandomTimerInterval(PUBLISH_RETRY_INTERVAL);

	setRandomTimerRange(0, PUBLISH_RETRY_INTERVAL);
}

MoodstripPublisher::~MoodstripPublisher()
{
	if (state != WaitForMoodstrip)
		goStop();
}

bool MoodstripPublisher::isWaitForMoodstrip() const
{
	return (state == WaitForMoodstrip);
}

void MoodstripPublisher::publishMoodstrip(qint32 id, const QString &caption, const QList<PublishMessageInfo> &messages, bool isHidden, qint32 recipientId)
{
	if (state != WaitForMoodstrip)
		return;

	clear();

	this->caption = caption;
	this->messages = messages;
	this->id = id;
	this->isHidden = isHidden;
	this->recipientId = recipientId;

	calculatePercentages();
	gotNewMoodstrip();
}

void MoodstripPublisher::start()
{
	gotStarted();
}

void MoodstripPublisher::stop()
{
	gotStopped();
}

void MoodstripPublisher::cleanup()
{
	gotCleanup();
}

void MoodstripPublisher::cancel()
{
	goStop();
}

void MoodstripPublisher::clear()
{
	messages.clear();
	moodstripId = -1;
	recipientId = -1;
	isHidden = false;
	
	caption.clear();
	urls.clear();
}

void MoodstripPublisher::calculatePercentages()
{
	qint64 totalSize = 0;

	QList <PublishMessageInfo>::iterator i = messages.begin();
	
	// Calculate the whole size first
	for (i = messages.begin(); i != messages.end(); ++i)
	{
		QFileInfo info((*i).imagePath);

		qint64 size = 0;

		if (info.exists())
		{
			size = info.size();
			totalSize += size;
		}
		
		(*i).setData(MESSAGE_INFO_SIZE, size);
	}

	// Now let's get cumulative percents for each item
	qint32 cumulativePercent = 0;

	for (i = messages.begin(); i != messages.end(); ++i)
	{
		qint64 size = (*i).getData(MESSAGE_INFO_SIZE).value<qint64>();

		if (size != 0 && totalSize != 0)
		{
			qint32 percent = (100 * size) / totalSize;

			cumulativePercent += percent;
		}

		(*i).setData(MESSAGE_INFO_PERCENT, cumulativePercent);
	}
}

void MoodstripPublisher::gotNewMoodstrip()
{
	if (state != WaitForMoodstrip)
		return;

	goCreateMoodstrip();
}

void MoodstripPublisher::gotStarted()
{
	MPDEBUG("MoodstripPublisher: User is online, resuming publishing");

	if (!paused)
		return;

	goContinue();
}

void MoodstripPublisher::gotStopped()
{
	MPDEBUG("MoodstripPublisher: User is offline, pausing publishing");
	
	if (paused)
		return;

	goPaused();
}

void MoodstripPublisher::gotTimerTimeout()
{
	if (state == WaitForMoodstrip)
		return;

	goDoOperation();
}

void MoodstripPublisher::gotCreationResponse()
{
	if (state != WaitForCreationResponse)
		return;

	clearTryNumber();

	goAddImage();
}

void MoodstripPublisher::gotCleanup()
{
	goStop();
}

void MoodstripPublisher::gotNetworkError()
{
	MPDEBUG("MoodstripPublisher: gotNetworkError");

	networkError();
}

void MoodstripPublisher::gotServerError()
{
	MPDEBUG("MoodstripPublisher: gotServerError");

	goStop();

	emit publishError(id);
}

// State transitions
void MoodstripPublisher::goCreateMoodstrip()
{
	state = WaitForCreationResponse;
	
	goDoOperation();

	MPDEBUG("MoodstripPublisher::state = WaitForCreationResponse");
}

void MoodstripPublisher::goAddImage()
{
	state = WaitForImageResponse;

	goDoOperation();

	MPDEBUG("MoodstripPublisher::state = WaitForImageResponse");
}

void MoodstripPublisher::goContinue()
{
	paused = false;

	goDoOperation();

	MPDEBUG("MoodstripPublisher - unpaused");
}

void MoodstripPublisher::goPaused()
{
	paused = true;

	stopTimer();
	clearTryNumber();

	MPDEBUG("MoodstripPublisher - paused");
}

void MoodstripPublisher::goDoOperation()
{
	switch (state)
	{
		case WaitForCreationResponse: createMoodstrip();
			break;
		
		case WaitForImageResponse: addImage(messages.size() <= 1);
			break;
	}
}

void MoodstripPublisher::goComplete()
{
	state = WaitForMoodstrip;

	MPDEBUG("MoodstripPublisher - completed " << moodstripId);

	emit publishCompleted(id, moodstripId, urls);
}

void MoodstripPublisher::goStop()
{
	goPaused();

	clear();

	state = WaitForMoodstrip;

	MPDEBUG("MoodstripPublisher - stopped");
}

void MoodstripPublisher::createMoodstrip()
{
	MPDEBUG("MoodstripPublisher - sent request for moodstrip id");
	
	SERVER->createMoodstrip(CALLBACK(this, onCreateMoodstripResult, qint32), caption, isHidden, recipientId);
}

void MoodstripPublisher::addImage(bool isLastPicture)
{
	PublishMessageInfo info = messages.first();

	MPDEBUG("MoodstripPublisher - sending file " << info.imagePath);

	QByteArray content;
	QString format;

	QFile file(info.imagePath);

	if (file.open(QIODevice::ReadOnly))
	{
		// Preparing content
		content = file.readAll();

		QImage image = QImage::fromData(content, DEFAULT_IMAGE_FORMAT);
		ImageTools::getSmallestImageContent(image, content, format);

		QString authorName;

		if (info.authorId == INFOMANAGER->getUserAccount().getId())
			authorName = INFOMANAGER->getUserAccount().getDisplayName();
		else
		{
			ContactInfo *authorContact = INFOMANAGER->getContact(info.authorId);

			if (authorContact != NULL)
				authorName = authorContact->getDisplayName();
			else
				authorName = info.authorLogin;
		}

		SERVER->addPictureToMoodstrip(CALLBACK(this, onAddPictureToMoodstripResult, PublishingMoodstripResult), moodstripId, info.messageId, authorName, content, format, isLastPicture);
	}
	else
	{
		imageAdded(QList<PublishingWay>());
	}
}

void MoodstripPublisher::moodstripCreated(qint32 id)
{
	moodstripId = id;

	MPDEBUG("MoodstripPublisher::moodstripCreated with id " << moodstripId);

	gotCreationResponse();
}

void MoodstripPublisher::imageAdded(const QList<PublishingWay> &urls)
{
	// Appears when stopping
	if (state != WaitForImageResponse)
		return;

	qint32 done = messages.first().getData(MESSAGE_INFO_PERCENT).toInt();
	MPDEBUG("MoodstripPublisher:: image added, work done " << done);

	messages.removeAt(0);

	clearTryNumber();

	if (messages.isEmpty())
	{
		this->urls = urls;

		goComplete();
	}
	else
		goDoOperation();
		
	emit publishing(id, done);
}

void MoodstripPublisher::networkError()
{
	if (!hasMoreTries())
	{
		MPDEBUG("MoodstripPublisher: No more tries, giving up at " << getRetryTimerInterval() << " try number " << getTryNumber());
		
		goStop();

		emit publishError(id);
	}
	else
	{
		MPDEBUG("MoodstripPublisher: Re-trying with interval " << getRetryTimerInterval());
		
		startNewTryTimer();
	}
}

void MoodstripPublisher::onTimerTimeout()
{
	goDoOperation();
}

void MoodstripPublisher::onCreateMoodstripResult(QVariant state, Fault fault, qint32 result)
{
	// Start of test code
	/*
	static TestCounter counter(2);
	
	if (counter.needMore())
		fault = Fault(FAULT_TRANSPORT_ERROR, "test", "test");
	*/
	// End of test code

	Q_UNUSED(state)
	
	MPDEBUG("MoodstripPublisher::onCreateMoodstripResult");

	if (!fault.isNull())
	{
		if (FaultTools::isNetworkError(fault))
		{
			gotNetworkError();
		}
		else
		{
			gotServerError();
		}
	}
	else
	{
		moodstripCreated(result);
	}
}

void MoodstripPublisher::onAddPictureToMoodstripResult(QVariant state, Fault fault, PublishingMoodstripResult result)
{
	Q_UNUSED(state)
	
	// Start of test code
	/*
	static TestCounter counter(5);
	
	if (counter.needMore())
		fault = Fault(FAULT_TRANSPORT_ERROR, "test", "test");
	*/
	// End of test code

	MPDEBUG("MoodstripPublisher::onAddPictureToMoodstripResult");

	if (!fault.isNull())
	{
		if (FaultTools::isNetworkError(fault))
		{
			gotNetworkError();
		}
		else
		{
			gotServerError();
		}
	}
	else
	{
		if (result.getResultCode() != MoodstripResultCode::Ok)
		{
			gotServerError();
		}
		else
		{
			imageAdded(result.getUrls());
		}
	}
}

// MessagePublisher class
MessagePublisher::MessagePublisher(QObject *parent)
	: QObject(parent)
{
	publisher = new MoodstripPublisher(this);

	connect(publisher, SIGNAL(publishing(qint32, qint32)), this, SIGNAL(publishing(qint32, qint32)));
	
	connect(publisher, SIGNAL(publishCompleted(qint32, qint32, const QList <PublishingWay> &)), this, SLOT(onPublishCompleted(qint32, qint32, const QList <PublishingWay> &)));
	connect(publisher, SIGNAL(publishError(qint32)), this, SLOT(onPublishError(qint32)));
}

void MessagePublisher::publish(qint32 id, const QString &caption, const QList<PublishMessageInfo> &messages, bool isHidden, qint32 recipientId)
{
	MPDEBUG("Added to publisher queue " << id);

	tasks.append(PublishTaskInfo(id, caption, messages, isHidden, recipientId));

	if (publisher->isWaitForMoodstrip())
		send(id);
}

void MessagePublisher::cancel(qint32 id)
{
	MPDEBUG("Cancelled from publisher queue " << id);

	if (publisher->getId() == id)
	{
		publisher->cancel();
	}

	remove(id);
}

void MessagePublisher::start()
{
	publisher->start();
}

void MessagePublisher::stop()
{
	publisher->stop();
}

void MessagePublisher::cleanup()
{
	tasks.clear();

	publisher->cleanup();
}

void MessagePublisher::send(qint32 id)
{
	MPDEBUG("Send to publisher worker " << id);

	for (int i = 0; i < tasks.size(); i++)
	{
		PublishTaskInfo task = tasks.at(i);
		if (task.id == id)
		{
			publisher->publishMoodstrip(task.id, task.caption, task.messages, task.isHidden, task.recipientId);
			break;
		}
	}
}

void MessagePublisher::remove(qint32 id)
{
	for (int i = 0; i < tasks.size(); i++)
	{
		if (tasks.at(i).id == id)
		{
			tasks.removeAt(i);
			break;
		}
	}
}

void MessagePublisher::onPublishCompleted(qint32 id, qint32 moodstripId, const QList <PublishingWay> &urls)
{
	MPDEBUG("Publish completed " << id);

	remove(id);

	if (!tasks.isEmpty())
		send(tasks.first().id);

	emit publishCompleted(id, moodstripId, urls);
}

void MessagePublisher::onPublishError(qint32 id)
{
	MPDEBUG("Publish error " << id);

	remove(id);

	if (!tasks.isEmpty())
		send(tasks.first().id);

	emit publishError(id);
}

}