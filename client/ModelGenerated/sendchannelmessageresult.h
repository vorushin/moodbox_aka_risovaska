#ifndef SENDCHANNELMESSAGERESULT_H
#define SENDCHANNELMESSAGERESULT_H

#include <QObject>
#include <QSharedData>

#include "sendmessageresult.h"
#include "transportableobject.h"
#include "fault.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class SendChannelMessageResultData : public QSharedData
{
public:
    SendChannelMessageResultData();
    SendChannelMessageResultData(SendMessageResult result);
    virtual ~SendChannelMessageResultData();

    SendMessageResult result;
};

class SendChannelMessageResult : public TransportableObject
{
public:
    SendChannelMessageResult();
    SendChannelMessageResult(SendMessageResult result);
    virtual ~SendChannelMessageResult();

protected:
    SendChannelMessageResult(SendChannelMessageResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static SendChannelMessageResult* ___new_()
    {
        return new SendChannelMessageResult(new SendChannelMessageResultData());
    }
    static SendChannelMessageResult empty()
    {
        return SendChannelMessageResult(new SendChannelMessageResultData());
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
    QExplicitlySharedDataPointer<SendChannelMessageResultData> d;
};

class SendChannelMessageResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, SendMessageResult result)
    {
       SendChannelMessageResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        SendChannelMessageResultCallbackCaller caller;
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
            CallbackCallerTools::onConnectFail("SendChannelMessageResult", SIGNAL(callbackSignal(QVariant, Fault, SendMessageResult)), callback.method);
        }
    }
};

}

#endif // SENDCHANNELMESSAGERESULT_H