#ifndef HTTPCHANNELCONTAINER_H
#define HTTPCHANNELCONTAINER_H

#include <QBuffer>
#include <QObject>
#include <QPointer>

#include "callback.h"
#include "fault.h"
#include "transportableobject.h"
#include "serverresponsehandler.h"

namespace MoodBox
{

// Very simple class to temporary store values between HTTP request and reply
class HttpChannelContainer: public QObject
{
	Q_OBJECT

public:
	ServerResponseHandler *handler;
	Callback callback;
	QVariant state;
	qint32 resultTypeId;
	QPointer<QBuffer> buffer;
	QPointer<QBuffer> outBuffer;
	Fault fault;

	HttpChannelContainer(ServerResponseHandler *handler, Callback &callback, QVariant state, qint32 resultTypeId, QBuffer *buffer);
	virtual ~HttpChannelContainer();

	bool getIsCancelled() const { return isCancelled; };
	void cancel(Fault fault) { this->fault = fault; isCancelled = true; };

public slots:
	// Proxy method to call handler's handleResponse method
	void handleResponse();

	// Trigger server error and callback with previously assigned Fault object
	void handleFault();

private:	
	bool isCancelled;
};

}

#endif // HTTPCHANNELCONTAINER_H
