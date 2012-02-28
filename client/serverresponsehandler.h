#ifndef SERVERRESPONSEHANDLER_H
#define SERVERRESPONSEHANDLER_H

#include "transportableobject.h"

namespace MoodBox
{

class Fault;

class ServerResponseHandler
{
public:
	// NotificationServerRegistrationFailed is unused currently
	enum ServerError { Disconnect = 0, UnsupportedClientVersion = 1, NotAuthenticated = 2, NotificationServerRegistrationFailed = 3 };
	
	virtual ~ServerResponseHandler() {};

public:
	virtual void handleResponse(Callback callback, QVariant state, qint32 resultTypeId, QIODevice &device) = 0;
	// Triggers specified server error, empty implementation for now
	virtual void triggerServerError(ServerError code) {Q_UNUSED(code);}

	virtual void resultFaultCall(Callback callback, QVariant state, Fault fault, qint32 resultTypeId) = 0;
};

}

#endif // SERVERRESPONSEHANDLER_H
