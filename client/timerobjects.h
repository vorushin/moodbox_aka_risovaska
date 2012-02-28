#ifndef TIMEROBJECTS_H
#define TIMEROBJECTS_H

#include <QObject>
#include <QTimer>

namespace MoodBox
{

// Utility class uses timer with single shot to call virtual slot
class SingleShotTimerObject : public QObject
{
	Q_OBJECT

public:
	SingleShotTimerObject(QObject *parent = 0);

protected:
	virtual void startTimer();
	virtual void stopTimer();

	virtual void setTimerInterval(qint32 interval);
	inline qint32 getTimerInterval() const { return timer.interval(); };

protected slots:
	virtual void onTimerTimeout() {};

private:
	QTimer timer;
	
};

// Timer class with retries counter
// Tries start from 1
class RetryTimerObject : public SingleShotTimerObject
{
	Q_OBJECT

public:
	RetryTimerObject(QObject *parent = 0);

protected:
	// Start timer with retry interval
	virtual void startNewTryTimer();

	// Set upper interval limit, 0 if no limit
	void setTimerIntervalLimit(qint32 limit);
	qint32 getTimerIntervalLimit() const { return limit; };

	// Get interval delta
	virtual qint32 getRetryTimerInterval();

	// Sets try number to 0
	void clearTryNumber();

	inline qint32 getTryNumber() const { return tryNumber; };

	// Check for more tries
	bool hasMoreTries();

private:
	qint32 tryNumber;

	qint32 limit;

};

// RandomRetryTimerObject
class RandomRetryTimerObject : public RetryTimerObject
{
	Q_OBJECT

public:
	RandomRetryTimerObject(QObject *parent = 0);

protected:
	void setRandomTimerInterval(qint32 interval);
	void setRandomTimerRange(qint32 min, qint32 max);

	virtual qint32 getRetryTimerInterval();	

private:
	qint32 randomTryNumber, nextRandomInterval, randomInterval;
	qint32 minRange, maxRange;
};

}

#endif // TIMEROBJECTS_H
