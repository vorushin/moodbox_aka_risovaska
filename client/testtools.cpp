#include "testtools.h"

#include "debug.h"

#ifdef SHOW_TIME

TimeMeasure::TimeMeasure()
{
	resetTime();

	isTimeShowed = false;
}

TimeMeasure::TimeMeasure(const QString &className)
{
	TimeDebug::getInstance()->debugStart("+ Start: " + className);

	resetTime();

	this->className = className;
	isTimeShowed = false;
}

TimeMeasure::~TimeMeasure()
{
	if (!isTimeShowed)
	{
		QString str("- Stop: " + className + "; time passed: %1");
		
		TimeDebug::getInstance()->debugStop(str.arg(time.elapsed()));
	}
}

void TimeMeasure::showTimePassed(const QString &message)
{
	if (className.isEmpty())
		QDEBUG("Time passed: " << time.elapsed() << " " << message);

	isTimeShowed = true;
}

void TimeMeasure::showTimePassedAfterSetupUi()
{
	QString str(className + "(setupUi)" + "; time passed: %1");
	
	TimeDebug::getInstance()->debugProcess(str.arg(time.elapsed()));
}

void TimeMeasure::resetTime()
{
	 time.start();
}

#else // SHOW_TIME

TimeMeasure::TimeMeasure()
{
}

TimeMeasure::TimeMeasure(const QString &className)
{
	Q_UNUSED(className);
}

TimeMeasure::~TimeMeasure()
{
}

void TimeMeasure::showTimePassed(const QString &message)
{
	Q_UNUSED(message);
}

void TimeMeasure::showTimePassedAfterSetupUi()
{
}

void TimeMeasure::resetTime()
{
}

#endif // SHOW_TIME



#ifdef SHOW_TIME

// TimeDebug class
TimeDebug* TimeDebug::timeDebug = NULL;
int TimeDebug::tabCount = 0;

TimeDebug* TimeDebug::getInstance()
{
	if (timeDebug == NULL)
		timeDebug = new TimeDebug;

	return timeDebug;
}

void TimeDebug::debugStart(QString message)
{
	QString tab;
	tab.fill(' ', tabCount * 4);

	tab.append(message);

	QDEBUG("" << tab);

	++tabCount;
}

void TimeDebug::debugProcess(QString message)
{
	QString tab;
	tab.fill(' ', (tabCount - 1) * 4);

	tab.append(message);

	QDEBUG("" << tab);
}

void TimeDebug::debugStop(QString message)
{
	--tabCount;

	QString tab;
	tab.fill(' ', tabCount * 4);

	tab.append(message);

	QDEBUG("" << tab);
}

#endif // SHOW_TIME