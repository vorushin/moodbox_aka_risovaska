#ifndef MESSAGEPUBLISHER_H
#define MESSAGEPUBLISHER_H

#include "timerobjects.h"

#include <QList>

#include "publishmessageinfo.h"
#include "userstatus.h"
#include "fault.h"

#include "moodstripresultcode.h"
#include "publishingmoodstripresult.h"

namespace MoodBox
{

// Publish maximum interval in msecs
#define PUBLISH_RETRY_MAXIMUM_INTERVAL	60000

// Publish timer interval in msecs
#define PUBLISH_RETRY_INTERVAL			5000

// Moodstrip publisher
class MoodstripPublisher : public RandomRetryTimerObject
{
	Q_OBJECT

public:
	MoodstripPublisher(QObject *parent = 0);
	virtual ~MoodstripPublisher();

	bool isWaitForMoodstrip() const;
	void publishMoodstrip(qint32 id, const QString &caption, const QList<PublishMessageInfo> &messages, bool isHidden, qint32 recipientId);

	void start();
	void stop();
	void cleanup();

	void cancel();

	qint32 getId() const { return id; };

signals:
	void publishing(qint32 id, qint32 percentDone);
	void publishCompleted(qint32 id, qint32 moodstripId, const QList <PublishingWay> &urls);

	void publishError(qint32 id);

private:
	/* Publisher's states:
		WaitForMoodstrip - nothing to do
		WaitForCreationResponse - wait for new moodstrip id response
		WaitForImageResponse - waiting for image adding response

	   State change drivers:
		1. Added moodstrip - need to create moodstrip id on server
		2. User status is Online - can start/resume publishing
		3. Moodstrip created on server - can start images adding
		4. Timer timeout - repeat current operation and wait for response
		5. Got server sent response - need to send next image
		6. Last image is sent with isLastPicture = true to publish moodstrip
		7. Got publish response - no more work
		8. User status is not Online - need to pause job (e.g. network error)
		9. User Account stopped - we need to cancel job

	   Publisher workflow:
		WaitForMoodstrip -> WaitForCreationResponse -> WaitForImageResponse -> WaitForMoodstrip

	*/

	enum State { WaitForMoodstrip, WaitForCreationResponse, WaitForImageResponse};
	State state;

	bool paused;

	QList <PublishMessageInfo> messages;

	qint32 id, moodstripId, recipientId;
	QString caption;
	QList <PublishingWay> urls;
	bool isHidden;

	// Events
	void gotNewMoodstrip();
	void gotStarted();
	void gotStopped();
	void gotTimerTimeout();
	void gotCreationResponse();
	void gotImageResponse();
	void gotCleanup();
	void gotNetworkError();
	void gotServerError();

	// State transitions
	void goCreateMoodstrip();
	void goAddImage();
	void goContinue();
	void goPaused();
	void goDoOperation();
	void goComplete();
	void goStop();

	// Calculate percentages
	void calculatePercentages();

	// Clear all data
	void clear();

	// Operations
	void createMoodstrip();
	void addImage(bool isLastPicture = false);
	
	void moodstripCreated(qint32 id);
	void imageAdded(const QList<PublishingWay> &urls);
	
	void networkError();

private slots:
	void onTimerTimeout();

	void onCreateMoodstripResult(QVariant state, Fault fault, qint32 result);
	void onAddPictureToMoodstripResult(QVariant state, Fault fault, PublishingMoodstripResult result);
};

// Utility class to keep publishing tasks
struct PublishTaskInfo
{
public:
	qint32 id, recipientId;
	QString caption;
	QList<PublishMessageInfo> messages;
	bool isHidden;

	PublishTaskInfo() : id(-1), recipientId(-1),  isHidden(false) {};
	PublishTaskInfo(qint32 id, const QString caption, const QList <PublishMessageInfo> &messages, bool isHidden, qint32 recipientId)
	{
		this->id = id;
		this->caption = caption;

		int counter = 0;

		foreach (PublishMessageInfo message, messages)
		{
			if (message.messageId <= 0)
				message.messageId = counter++;

			this->messages.append(message);
		}

		this->isHidden = isHidden;
		this->recipientId = recipientId;
	};
};

// Message publisher
class MessagePublisher : public QObject
{
	Q_OBJECT

public:
	MessagePublisher(QObject *parent = 0);

	void publish(qint32 id, const QString &caption, const QList<PublishMessageInfo> &messages, bool isHidden, qint32 recipientId);
	void cancel(qint32 id);

	void start();
	void stop();
	void cleanup();

signals:
	void publishing(qint32 id, qint32 percentDone);
	void publishCompleted(qint32 id, qint32 moodstripId, const QList<PublishingWay> &urls);

	void publishError(qint32 id);

private:
	MoodstripPublisher *publisher;

	QList <PublishTaskInfo> tasks;

	void send(qint32 id);
	void remove(qint32 id);

private slots:
	void onPublishCompleted(qint32 id, qint32 moodstripId, const QList<PublishingWay> &urls);
	void onPublishError(qint32 id);
};

}
#endif // MESSAGEPUBLISHER_H