#include "httpchannel.h"

#include <QFile>
#include <QHttp>
#include <QSslConfiguration>
#include <QThread>

#include "debug.h"
#include "fault.h"
#include "serverresponsehandler.h"
#include "xmlpropertywriter.h"

namespace MoodBox
{

HttpChannel::HttpChannel(QUrl serverUrl) : serverUrl(serverUrl)
{
	setRequestSizeLimit(DEFAULT_REQUEST_SIZE_LIMIT);

	// Ignore invalid peer SSL errors
	QSslConfiguration config = QSslConfiguration::defaultConfiguration();
	config.setPeerVerifyMode(QSslSocket::VerifyNone);
	QSslConfiguration::setDefaultConfiguration(config);

	http = new QHttp(serverUrl.host(),
		serverUrl.scheme() == "https" ? QHttp::ConnectionModeHttps : QHttp::ConnectionModeHttp, 
		serverUrl.port(0), // We need to specify 0 port as default if no port is specified
		this);
	connect(http, SIGNAL(requestFinished(int, bool)), this, SLOT(requestFinished(int, bool)));
	connect(http, SIGNAL(dataReadProgress(int, int)), this, SLOT(dataProcessedProgress(int, int)));
	connect(http, SIGNAL(dataSendProgress(int, int)), this, SLOT(dataProcessedProgress(int, int)));

	// Init request timer
	requestTimer.setSingleShot(true);
	setRequestTimeout(DEFAULT_REQUEST_TIMEOUT);
	connect(&requestTimer, SIGNAL(timeout()), this, SLOT(onAbort()));
}

void HttpChannel::send(Model* model, ServerResponseHandler *handler, Callback callback, QVariant state, TransportableObject* object, qint32 resultTypeId)
{
	Q_ASSERT_X(thread() == QThread::currentThread(), "HttpChannel::send", "Current thread must be object's thread");

	QBuffer *buffer = new QBuffer(this);
	buffer->open(QIODevice::ReadWrite);

	XmlPropertyWriter::write(model, buffer, object);
	delete object;
	buffer->seek(0);

	HttpChannelContainer *container = new HttpChannelContainer(handler, callback, state, resultTypeId, buffer);
	if (buffer->buffer().size() > requestSizeLimit) // Check for too large messages
	{
		faultCall(container, Fault(FAULT_REQUEST_TOO_LARGE, QString(), QString()));
		return;
	}

	pendingRequests.enqueue(container);
	sendPendingRequest();
}

void HttpChannel::setRequestTimeout(int newTimeout)
{
	requestTimeout = newTimeout;
	requestTimer.setInterval(requestTimeout * 1000);
}

void HttpChannel::setRequestSizeLimit(int newLimit)
{
	requestSizeLimit = newLimit;
}

void HttpChannel::abort()
{
	http->abort();
}

void HttpChannel::sendPendingRequest()
{
	Q_ASSERT_X(thread() == QThread::currentThread(), "HttpChannel::send", "Current thread must be object's thread");

	// We're assuming HTTP communication is done in GUI thread, otherwise we need QMutex here
	if (currentRequest.second || pendingRequests.isEmpty())
		return;

	HttpChannelContainer *container = pendingRequests.dequeue();

	int requestId = http->post(serverUrl.path(), container->buffer->buffer(), container->outBuffer);

	QDEBUG("HttpChannel::sendPendingRequest: requestId = " << requestId << ", method = " << (container->callback.method));

	currentRequest = qMakePair(requestId, container);
	requestTimer.start();

#ifndef QT_NO_DEBUG
	QFile file(QString("Logs/request") + QString::number(requestId) + ".xml");
	file.open(QIODevice::WriteOnly);
	file.write(container->buffer->buffer());
	file.close();
	container->buffer->seek(0);
#endif
}

void HttpChannel::faultCall(HttpChannelContainer *container, Fault fault)
{
	container->fault = fault;
	QTimer::singleShot(0, container, SLOT(faultCall()));
}

void HttpChannel::requestFinished(int requestId, bool isError)
{
	// WARNING!!! DON'T PUT BREAKPOINTS IN THIS METHOD UNTIL ALL REQUEST IS RECEIVED!!! Better don't use breakpoints here at all :) /Denis

	QDEBUG("HttpChannel::requestFinished: requestId = " << requestId << ", isError = " << isError);

	if (requestId != currentRequest.first)
	{
		QDEBUG("HttpChannel::requestFinished: unknown requestId = " << requestId << ", expected = " << currentRequest.first);
		return;
	}

	requestTimer.stop();

	HttpChannelContainer *container = currentRequest.second;
	currentRequest.first = 0;
	currentRequest.second = NULL;

	if (!isError)
	{
#ifndef QT_NO_DEBUG
		QFile file(QString("Logs/response") + QString::number(requestId) + ".xml");
		file.open(QIODevice::WriteOnly);
		file.write(container->outBuffer->buffer());
		file.close();
#endif
		QTimer::singleShot(0, container, SLOT(handleResponse()));
	}
	else
	{
		faultCall(container, Fault(FAULT_TRANSPORT_ERROR, http->errorString(), ""));
		QDEBUG("HttpChannel::requestFinished: closing connection for requestId = " << requestId);
		http->close(); // For any case, helps when resuming from suspend or hibernate
	}
	// container will delete itself

	sendPendingRequest();
}

void HttpChannel::dataProcessedProgress(int done, int total)
{
	Q_UNUSED(done);
	Q_UNUSED(total);
	requestTimer.start();
}

void HttpChannel::onAbort()
{
	// under high CPU load here we often have requestId != currentRequest.first
	// sometimes requestId == 0, but sometimes not 8-O
	// and all become broken :-(

	int requestId = http->currentId();
	QDEBUG("HttpChannel::onAbort: request " << requestId << " will be aborted, current requestId = " << currentRequest.first);
	abort();

	if(requestId == 0 && currentRequest.second)
	{
		QDEBUG("HttpChannel::onAbort: cleaning up request" << currentRequest.first);

		requestTimer.stop();

		HttpChannelContainer *container = currentRequest.second;
		currentRequest.first = 0;
		currentRequest.second = NULL;

		faultCall(container, Fault(FAULT_TRANSPORT_ERROR, "Request timed out", ""));
		http->close(); // For any case, helps when resuming from suspend or hibernate

		sendPendingRequest();
	}
}

}