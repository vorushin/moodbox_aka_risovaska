#ifndef TRANSPORTCHANNELBASE_H
#define TRANSPORTCHANNELBASE_H

#include <QObject>

#include "callback.h"

namespace MoodBox
{

class Model;
class ServerResponseHandler;
class TransportableObject;

class TransportChannelBase : public QObject
{
	Q_OBJECT

public:
	virtual void send(Model* model, ServerResponseHandler *handler, Callback callback, QVariant state, 
		TransportableObject* object, qint32 resultTypeId) = 0;
	virtual void setRequestTimeout(int newTimeout) = 0;
	virtual void setRequestSizeLimit(qint32 newLimit) = 0;
	virtual void abort() = 0;
	virtual void cancelAll() = 0;

protected:
	int requestTimeout;
};

}

#endif // TRANSPORTCHANNELBASE_H
