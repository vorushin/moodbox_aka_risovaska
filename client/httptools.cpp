#include "httptools.h"

#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QApplication>

namespace MoodBox
{

QNetworkAccessManager *HttpLoader::httpLoaderManager = NULL;

HttpLoader::HttpLoader(QObject *parent)
	: RandomRetryTimerObject(parent), state(Ready), stopRequested(false), reply(NULL)
{
	setTimerIntervalLimit(HTTPLOADER_RETRY_MAXIMUM_INTERVAL);
	setRandomTimerInterval(HTTPLOADER_RETRY_INTERVAL);
	setRandomTimerRange(0, HTTPLOADER_RETRY_INTERVAL);

	if (httpLoaderManager == NULL)
		httpLoaderManager = new QNetworkAccessManager(qApp);

	connect(httpLoaderManager, SIGNAL(finished(QNetworkReply *)), this, SLOT(onNetworkReply(QNetworkReply *)));
}

QNetworkReply *HttpLoader::get(QNetworkRequest &request)
{
	this->request = request;

	return reGet();
}

QNetworkReply *HttpLoader::reGet()
{
	if (state != Ready)
		return NULL;

	clearTryNumber();

	return sendRequest();
}

void HttpLoader::stop()
{
	switch (state)
	{
		case WaitingForReply:
			stopRequested = true;

			if (reply != NULL)
				reply->abort();
			
			state = Ready;
			break;

		case WaitingForTimer:
			stopTimer();

			break;
	}
}

void HttpLoader::startTimer()
{
	RandomRetryTimerObject::startTimer();
	state = WaitingForTimer;
}

void HttpLoader::stopTimer()
{
	RandomRetryTimerObject::stopTimer();
	state = Ready;
}

void HttpLoader::onTimerTimeout()
{
	state = Ready;

	if (!hasMoreTries())
	{
		emit finished(NULL);
	}
	else
	{
		sendRequest();
	}
}

void HttpLoader::onNetworkReply(QNetworkReply *reply)
{
	if (reply != this->reply)
		return;

	state = Ready;

	if (reply->error() == QNetworkReply::NoError)
	{
		emit finished(reply);
	}
	else
		if (!stopRequested)
		{
			startNewTryTimer();		
		}
		else
			stopRequested = false;
}

QNetworkReply *HttpLoader::sendRequest()
{
	reply = httpLoaderManager->get(request);
	state = WaitingForReply;
	
	return reply;
}

}