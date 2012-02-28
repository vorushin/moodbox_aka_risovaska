#ifndef SENDPRIVATEMESSAGERESULT_H
#define SENDPRIVATEMESSAGERESULT_H

#include <QObject>
#include <QSharedData>

#include "sendmessageresult.h"
#include "transportableobject.h"
#include "fault.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class SendPrivateMessageResultData : public QSharedData
{
public:
    SendPrivateMessageResultData();
    SendPrivateMessageResultData(SendMessageResult result);
    virtual ~SendPrivateMessageResultData();

    SendMessageResult result;
};

class SendPrivateMessageResult : public TransportableObject
{
public:
    SendPrivateMessageResult();
    SendPrivateMessageResult(SendMessageResult result);
    virtual ~SendPrivateMessageResult();

protected:
    SendPrivateMessageResult(SendPrivateMessageResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static SendPrivateMessageResult* ___new_()
    {
        return new SendPrivateMessageResult(new SendPrivateMessageResultData());
    }
    static SendPrivateMessageResult empty()
    {
        return SendPrivateMessageResult(new SendPrivateMessageResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    SendMessageResult getResult() const;
    void setResult(SendMessageResult value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<SendPrivateMessageResultData> d;
};

class SendPrivateMessageResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, SendMessageResult result)
    {
       SendPrivateMessageResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        SendPrivateMessageResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, SendMessageResult());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, SendMessageResult result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, SendMessageResult result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, SendMessageResult)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("SendPrivateMessageResult", SIGNAL(callbackSignal(QVariant, Fault, SendMessageResult)), callback.method);
        }
    }
};

}

#endif // SENDPRIVATEMESSAGERESULT_H