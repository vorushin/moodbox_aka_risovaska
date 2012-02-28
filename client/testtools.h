#ifndef TESTTOOLS_H
#define TESTTOOLS_H

#include <QTime>
#include <QString>

//#define SHOW_TIME

// Simple try counter class
class TestCounter
{
public:
	TestCounter(int count) : count(count), now(0) {};

	bool needMore() { return (++now < count); };

	inline int getCount() const { return count; };
	inline int getNow() const { return now; };

private:
	int count, now;
};

// Class to measure the time before calls
class TimeMeasure
{
public:
	TimeMeasure();
	TimeMeasure(const QString &className);
	~TimeMeasure();

	void showTimePassed(const QString &message = QString());
	void showTimePassedAfterSetupUi();
	void resetTime();

private:
	QTime time;
	QString className;
	bool isTimeShowed;
};

#ifdef SHOW_TIME

// Class helper for debug output with indent
class TimeDebug
{
public:
	static TimeDebug* getInstance();

	static void debugStart(QString message);
	static void debugProcess(QString message);
	static void debugStop(QString message);

private:
	static TimeDebug *timeDebug;
	static int tabCount;
};

#endif // SHOW_TIME

#endif // TESTTOOLS_H