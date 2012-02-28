#include "httpchannel2.h"

#include <QFile>
#include <QSslConfiguration>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QThread>

#include "debug.h"
#include "fault.h"
#include "serverresponsehandler.h"
#include "xmlpropertywriter.h"

namespace MoodBox
{

int HttpChannel2::globalRequestId = 0;

HttpChannel2::HttpChannel2(QUrl serverUrl) : serverUrl(serverUrl), http(NULL), requestToHandle(NULL)
{
	setRequestSizeLimit(DEFAULT_REQUEST_SIZE_LIMIT);

	// Ignore invalid peer SSL errors
	QSslConfiguration config = QSslConfiguration::defaultConfiguration();
	config.setPeerVerifyMode(QSslSocket::VerifyNone);
	QSslConfiguration::setDefaultConfiguration(config);

	initHttp();

	// Init request timer
	requestTimer.setSingleShot(true);
	setRequestTimeout(DEFAULT_REQUEST_TIMEOUT);
	connect(&requestTimer, SIGNAL(timeout()), this, SLOT(onAbort()));
}

void HttpChannel2::send(Model* model, ServerResponseHandler *handler, Callback callback, QVariant state, TransportableObject* object, qint32 resultTypeId)
{
	Q_ASSERT_X(thread() == QThread::currentThread(), "HttpChannel2::send", "Current thread must be object's thread");

	QBuffer *buffer = new QBuffer(this);
	buffer->open(QIODevice::ReadWrite);

	XmlPropertyWriter::write(model, buffer, object);
	delete object;
	buffer->seek(0);

	HttpChannelContainer *container = new HttpChannelContainer(handler, callback, state, resultTypeId, buffer);
	if (buffer->buffer().size() > requestSizeLimit) // Check for too large messages
	{
		riseFault(container, Fault(FAULT_REQUEST_TOO_LARGE, QString(), QString()), true);
		return;
	}

	pendingRequests.enqueue(container);
	sendPendingRequest();
}

void HttpChannel2::setRequestTimeout(int newTimeout)
{
	requestTimeout = newTimeout;
	requestTimer.setInterval(requestTimeout * 1000);
}

void HttpChannel2::setRequestSizeLimit(int newLimit)
{
	requestSizeLimit = newLimit;
}

void HttpChannel2::abort()
{
	abort(getCancelledFault());
}

void HttpChannel2::abort(Fault fault)
{
	if (currentRequest.first)
	{
		QDEBUG("HttpChannel::abort: message #" << requestId << "is aborted");
		currentRequest.second->cancel(fault);
		currentRequest.first->abort();

		// NOTE workaround of Qt bug: killing QNetworkAccessManager to avoid crash
		initHttp();
	}
}

void HttpChannel2::cancelAll()
{
	abort();

	if(requestToHandle != NULL)
		requestToHandle->cancel(getCancelledFault());

	while (pendingRequests.size() > 0)
	{
		riseFault(pendingRequests.takeFirst(), getCancelledFault(), true);
	}
}

void HttpChannel2::riseFault(HttpChannelContainer *container, bool delayed)
{
	if(delayed)
		QTimer::singleShot(0, container, SLOT(handleFault()));
	else
		container->handleFault();
}

void HttpChannel2::riseFault(HttpChannelContainer *container, Fault fault, bool delayed)
{
	container->fault = fault;
	riseFault(container, delayed);
}

Fault HttpChannel2::getCancelledFault()
{
	return Fault(FAULT_REQUEST_CANCELLED, QString(), QString());
}

void HttpChannel2::initHttp()
{
	if(http != NULL)
	{
		QDEBUG("HttpChannel2::initHttp killing http");
		http->disconnect();
		delete http; // deleteLater() can be too late
	}

	http = new QNetworkAccessManager(this);
	connect(http, SIGNAL(finished(QNetworkReply *)), this, SLOT(requestFinished(QNetworkReply *)));
}

void HttpChannel2::sendPendingRequest()
{
	Q_ASSERT_X(thread() == QThread::currentThread(), "HttpChannel2::send", "Current thread must be object's thread");

	// We're assuming HTTP communication is done in GUI thread, otherwise we need QMutex here
	if (currentRequest.second || pendingRequests.isEmpty())
		return;

	globalRequestId++;
	requestId = globalRequestId;

	HttpChannelContainer *container = pendingRequests.dequeue();

	QNetworkRequest request(serverUrl);

#ifndef QT_NO_DEBUG
	request.setRawHeader("X-Request-Id", QString::number(requestId).toAscii());
#endif

	QNetworkReply *reply = http->post(request, container->buffer->buffer());

	connect(reply, SIGNAL(downloadProgress(qint64, qint64)), this, SLOT(dataProcessedProgress(qint64, qint64)));
	connect(reply, SIGNAL(uploadProgress(qint64, qint64)), this, SLOT(dataProcessedProgress(qint64, qint64)));
	
	requestTimer.start();

	currentRequest = qMakePair(reply, container);

/*#ifndef QT_NO_DEBUG
	QFile file(QString("Logs/request") + QString::number(requestId) + ".xml");
	file.open(QIODevice::WriteOnly);
	file.write(container->buffer->buffer());
	file.close();
	container->buffer->seek(0);
#endif*/
}

void HttpChannel2::requestFinished(QNetworkReply *reply)
{
#ifndef QT_NO_DEBUG
	// here is a known problem with QNetworkAccessManager: aborted request sometimes fires "finished" twice
	// current strategy is to ignore unknown requests and seems to be valid workaround
//	QByteArray assertData = reply->request().rawHeader("X-Request-Id");
//	QDEBUG("HttpChannel2::requestFinished requestId = " << assertData << ", reply = " << reply);
//	Q_ASSERT_X(reply == currentRequest.first, "HttpChannel2::requestFinished", (QString("Unknown reply with request id = ") + assertData).toLatin1());
#endif

	if (reply != currentRequest.first)
	{
		reply->deleteLater();
		return;
	}

	requestTimer.stop();

	HttpChannelContainer *container = currentRequest.second;
	currentRequest.first = 0;
	currentRequest.second = NULL;

	if(container->getIsCancelled())
	{
		QDEBUG("HttpChannel2::requestFinished cancelling " << container->callback.method << ", fault = " << container->fault.getCode());
		riseFault(container, true);
		QTimer::singleShot(0, this, SLOT(sendPendingRequest()));
	}
	else 
	{
		if (reply->error() == QNetworkReply::NoError)
		{
			QByteArray data = reply->readAll();
			container->outBuffer->open(QIODevice::ReadWrite);
			container->outBuffer->write(data);
			container->outBuffer->seek(0);
		}
		else
		{
			container->fault = Fault(FAULT_TRANSPORT_ERROR, reply->errorString(), "");
/*#ifndef QT_NO_DEBUG
			QFile file(QString("Logs/response") + QString::number(requestId) + ".xml");
			file.open(QIODevice::WriteOnly);
			file.write(reply->errorString().toAscii());
			file.close();
#endif*/
		}

		requestToHandle = container;
		QTimer::singleShot(0, this, SLOT(handleAndContinue()));
		// do not send next request, it will be done by handleAndContinue()
	}

	reply->deleteLater();
}

void HttpChannel2::handleAndContinue()
{
	// save requestToHandle to request before other code can replace it
	HttpChannelContainer *request = requestToHandle;
	requestToHandle = NULL;

	sendPendingRequest();

	if(request->getIsCancelled())
	{
		QDEBUG("HttpChannel2::handleAndContinue cancelling " << request->callback.method << ", fault = " << request->fault.getCode());
		riseFault(request, false);
	}
	else
	{
		if(request->fault.isNull())
			request->handleResponse();
		else
			request->handleFault();
	}
}

void HttpChannel2::dataProcessedProgress(qint64 done, qint64 total)
{
	Q_UNUSED(done);
	Q_UNUSED(total);
	requestTimer.start();
}

void HttpChannel2::onAbort()
{
	abort(Fault(FAULT_REQUEST_TIMED_OUT, QString(), QString()));
}

}