#ifndef CALLBACKCALLERTOOLS_H
#define CALLBACKCALLERTOOLS_H

namespace MoodBox
{

class CallbackCallerTools
{
public:
	// Handles failed connect() call
	static void onConnectFail(const char *callerName, const char *signal, const char *slot);
};

}

#endif // CALLBACKCALLERTOOLS_H
