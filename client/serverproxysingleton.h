#ifndef SERVERPROXYSINGLETON_H
#define SERVERPROXYSINGLETON_H

#include "moodboxcustomserver.h"

namespace MoodBox
{
// Handy macroses to call server proxy methods.  Like this:  SERVER->getUserPicture(CALLBACK(this, onGetPictureResult, UserPictureResult), id, lastDate)
#define SERVER ServerProxySingleton::getInstance()
#define CALLBACK(Target, Method, ...) Callback(Target, SLOT(Method(QVariant, Fault, __VA_ARGS__))) // third parameter is result value type
#define CALLBACK_NOARGS(Target, Method) Callback(Target, SLOT(Method(QVariant, Fault)))

class HttpChannel2;
class MoodBoxModel;

/*
Class with only static method to get access to singleton MoodBoxCustomServer instance.
Use SERVER macro to call server proxy methods.
*/
class ServerProxySingleton
{
public:
	static MoodBoxCustomServer* getInstance();

private:
	static MoodBoxModel* model;
	static MoodBoxCustomServer* singleton;
	static HttpChannel2* channel;
};

}

#endif // SERVERPROXYSINGLETON_H