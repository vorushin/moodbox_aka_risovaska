#include "timerobjects.h"

#include "mtrandom.h"

namespace MoodBox
{

MTRandom _randomRetryRandom;

// SingleShotTimerObject class
SingleShotTimerObject::SingleShotTimerObject(QObject *parent)
	: QObject(parent)
{
	timer.setSingleShot(true);
	
	connect(&timer, SIGNAL(timeout()), this, SLOT(onTimerTimeout()));
}

void SingleShotTimerObject::startTimer()
{
	if (timer.isActive())
		return;

	timer.start();
}

void SingleShotTimerObject::stopTimer()
{
	if (!timer.isActive())
		return;

	timer.stop();
}

void SingleShotTimerObject::setTimerInterval(qint32 interval)
{
	timer.setInterval(qMax(interval, 0));
}

// RetryTimerObject class
RetryTimerObject::RetryTimerObject(QObject *parent)
	: SingleShotTimerObject(parent), tryNumber(1), limit(0)
{
}

void RetryTimerObject::startNewTryTimer()
{
	setTimerInterval(getRetryTimerInterval());
	startTimer();

	tryNumber++;
}

void RetryTimerObject::setTimerIntervalLimit(qint32 limit)
{
	this->limit = limit;
}

qint32 RetryTimerObject::getRetryTimerInterval()
{
	return tryNumber * getTimerInterval();
}

void RetryTimerObject::clearTryNumber()
{
	tryNumber = 1;
}

bool RetryTimerObject::hasMoreTries()
{
	return (limit > 0) ? getRetryTimerInterval() < limit : true;
}

// RandomRetryTimerObject class
RandomRetryTimerObject::RandomRetryTimerObject(QObject *parent)
	: RetryTimerObject(parent), randomTryNumber(0), nextRandomInterval(0), randomInterval(0), minRange(0), maxRange(0)
{
}

void RandomRetryTimerObject::setRandomTimerInterval(qint32 interval)
{
	randomInterval = interval;
}

void RandomRetryTimerObject::setRandomTimerRange(qint32 min, qint32 max)
{
	minRange = min;
	maxRange = max;
}

qint32 RandomRetryTimerObject::getRetryTimerInterval()
{
	if (randomTryNumber != getTryNumber())
	{
		randomTryNumber = getTryNumber();
		nextRandomInterval = getTryNumber() * _randomRetryRandom.getNextIntRanged(randomInterval - minRange, randomInterval + maxRange);

		if (getTimerIntervalLimit() != 0 && nextRandomInterval > getTimerIntervalLimit())
			nextRandomInterval = getTimerIntervalLimit();	
	}

	return nextRandomInterval;
}

}