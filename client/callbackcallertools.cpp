#include "callbackcallertools.h"

#include <QString>

namespace MoodBox
{

void CallbackCallerTools::onConnectFail(const char *callerName, const char *signal, const char *slot)
{
#ifndef QT_NO_DEBUG 
	throw QString(callerName) + " couldn't connect " + signal + " to " + slot;
#else
	Q_UNUSED(callerName);
	Q_UNUSED(signal);
	Q_UNUSED(slot);
#endif
}

}