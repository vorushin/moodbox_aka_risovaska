#ifndef CALLBACK_H
#define CALLBACK_H

#include <QMetaType>
#include <QObject>

namespace MoodBox
{

struct Callback
{
	Callback(QObject* target, const char * method)
	{
		this->target = target;
		this->method = method;
	}

	Callback()
	{
		target = NULL;
		method = NULL;
	}

	QObject* target;
	const char * method;
};

class Int32CallbackCaller : public QObject
{
	Q_OBJECT

public:
	static void call(Callback &callback, int result)
	{
		Int32CallbackCaller caller;
		caller.callInternal(callback, result);
	}

signals:
	void callbackSignal(int result);

private:
	void callInternal(Callback &callback, int result)
	{
		connect(this, SIGNAL(callbackSignal(int)), callback.target, callback.method);
		emit callbackSignal(result);
		disconnect();
	}
};

//	Callback c(&t, SLOT(onResult(int)));

//	Int32CallbackCaller::call(c, 123);


}

Q_DECLARE_METATYPE(MoodBox::Callback)

#endif // CALLBACK_H
