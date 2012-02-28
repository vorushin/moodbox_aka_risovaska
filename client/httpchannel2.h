#ifndef HTTPCHANNEL2_H
#define HTTPCHANNEL2_H

#include <QQueue>
#include <QTimer>
#include <QUrl>

#include "httpchannelcontainer.h"
#include "transportchannelbase.h"

class QNetworkAccessManager;
class QNetworkReply;

// Default HTTP request timeout, in seconds
#define DEFAULT_REQUEST_TIMEOUT 30

// Default HTTP message size limit, in bytes
#define DEFAULT_REQUEST_SIZE_LIMIT 2000000

namespace MoodBox
{

/*
Performs sending of server request and handling of its response via HTTP channel.
*/
class HttpChannel2 : public TransportChannelBase
{
	Q_OBJECT

public:
	HttpChannel2(QUrl serverUrl);

	virtual void send(Model* model, ServerResponseHandler *handler, Callback callback, QVariant state, TransportableObject* object, qint32 resultTypeId);

	// Sets new request timeout (note it will work beginning from the next request, not current one)
	virtual void setRequestTimeout(int newTimeout);

	virtual void setRequestSizeLimit(qint32 newLimit);

	// Forcibly aborts current request
	virtual void abort();
	virtual void abort(Fault fault);

	// Cancels all requests including queued
	virtual void cancelAll();
	void riseFault(HttpChannelContainer *container, bool delayed);
	void riseFault(HttpChannelContainer *container, Fault fault, bool delayed);

protected:
	QUrl serverUrl;
	QNetworkAccessManager *http;
	// Current active request, we're handling only request at a time
	QPair<QNetworkReply*, HttpChannelContainer*> currentRequest;
	HttpChannelContainer *requestToHandle;
	QQueue<HttpChannelContainer*> pendingRequests;
	qint32 requestSizeLimit;
	static int globalRequestId;
	int requestId;
	int requestTimeout;
	QTimer requestTimer;

	Fault getCancelledFault();
	void initHttp();

private slots:
	// Sends next pending request, if any
	void sendPendingRequest();
	void requestFinished(QNetworkReply *reply);
	void handleAndContinue();
	void dataProcessedProgress(qint64 done, qint64 total);
	void onAbort();
};

}

#endif // HTTPCHANNEL2_H
