#ifndef SERVERPROXYBASE_H
#define SERVERPROXYBASE_H

#include <QVariant>

#include "callback.h"
#include "serverresponsehandler.h"

#include "faultcodes.h"

namespace MoodBox
{

class Header;
class Model;
class TransportableObject;
class TransportChannelBase;

class ServerProxyBase : public ServerResponseHandler
{
public:
	ServerProxyBase(Model* model, TransportChannelBase* channel);
	virtual ~ServerProxyBase();

protected:
	void send(Callback callback, QVariant state, TransportableObject* object);
	virtual void handleResponse(Callback callback, QVariant state, qint32 resultTypeId, QIODevice &device);
	virtual Header getHeader();

	Model* model;
	TransportChannelBase* channel;
};

}

#endif // SERVERPROXYBASE_H
