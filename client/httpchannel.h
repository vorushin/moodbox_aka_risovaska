#ifndef HTTPCHANNEL_H
#define HTTPCHANNEL_H

#include <QQueue>
#include <QTimer>
#include <QUrl>

#include "httpchannelcontainer.h"
#include "transportchannelbase.h"

class QHttp;

// Default HTTP request timeout, in seconds
#define DEFAULT_REQUEST_TIMEOUT 30

// Default HTTP message size limit, in bytes
#define DEFAULT_REQUEST_SIZE_LIMIT 2000000

namespace MoodBox
{

/*
Performs sending of server request and handling of its response via HTTP channel.
*/
class HttpChannel : public TransportChannelBase
{
	Q_OBJECT

public:
	HttpChannel(QUrl serverUrl);

	virtual void send(Model* model, ServerResponseHandler *handler, Callback callback, QVariant state, TransportableObject* object, qint32 resultTypeId);

	// Sets new request timeout (note it will work beginning from the next request, not current one)
	virtual void setRequestTimeout(int newTimeout);

	virtual void setRequestSizeLimit(qint32 newLimit);

	// Forcibly aborts current request
	virtual void abort();

protected:
	QUrl serverUrl;
	QHttp *http;
	// Current active request, we're handling only request at a time
	QPair<int, HttpChannelContainer*> currentRequest;
	QQueue<HttpChannelContainer*> pendingRequests;
	int requestTimeout;
	QTimer requestTimer;
	qint32 requestSizeLimit;

protected:
	// Sends next pending request, if any
	void sendPendingRequest();
	void faultCall(HttpChannelContainer *container, Fault fault);

private slots:
	void requestFinished(int requestId, bool isError);
	void dataProcessedProgress(int done, int total);
	void onAbort();
};

}

#endif // HTTPCHANNEL_H
