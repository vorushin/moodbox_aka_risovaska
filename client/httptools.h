#ifndef HTTPTOOLS_H
#define HTTPTOOLS_H

#include "timerobjects.h"

#include <QNetworkRequest>

class QNetworkAccessManager;
class QNetworkReply;

namespace MoodBox
{

// Maximum timeout for retries is 30 sec (1 * 30 * 1000 msec)
#define HTTPLOADER_RETRY_MAXIMUM_INTERVAL			30000

// Loader timeout for retries is 2 sec
#define HTTPLOADER_RETRY_INTERVAL					2000

// Class for multiple retries to get specific request
class HttpLoader: public RandomRetryTimerObject
{
	Q_OBJECT

public:
	HttpLoader(QObject *parent = 0);

	QNetworkReply *get(QNetworkRequest &request);
	QNetworkReply *reGet();

	void stop();

signals:
	void finished(QNetworkReply *reply);

protected:
	virtual void startTimer();
	virtual void stopTimer();

protected slots:
	virtual void onTimerTimeout();

	void onNetworkReply(QNetworkReply *reply);

private:
	enum State {Ready, WaitingForReply, WaitingForTimer};
	State state;

	bool stopRequested;

	QNetworkRequest request;
	QNetworkReply *reply;

	QNetworkReply *sendRequest();

	static QNetworkAccessManager *httpLoaderManager;
};

}
#endif // HTTPTOOLS_H