#ifndef SENDFRIENDMESSAGERESULT_H
#define SENDFRIENDMESSAGERESULT_H

#include <QObject>
#include <QSharedData>

#include "sendmessageresult.h"
#include "transportableobject.h"
#include "fault.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class SendFriendMessageResultData : public QSharedData
{
public:
    SendFriendMessageResultData();
    SendFriendMessageResultData(SendMessageResult result);
    virtual ~SendFriendMessageResultData();

    SendMessageResult result;
};

class SendFriendMessageResult : public TransportableObject
{
public:
    SendFriendMessageResult();
    SendFriendMessageResult(SendMessageResult result);
    virtual ~SendFriendMessageResult();

protected:
    SendFriendMessageResult(SendFriendMessageResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static SendFriendMessageResult* ___new_()
    {
        return new SendFriendMessageResult(new SendFriendMessageResultData());
    }
    static SendFriendMessageResult empty()
    {
        return SendFriendMessageResult(new SendFriendMessageResultData());
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
    QExplicitlySharedDataPointer<SendFriendMessageResultData> d;
};

class SendFriendMessageResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, SendMessageResult result)
    {
       SendFriendMessageResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        SendFriendMessageResultCallbackCaller caller;
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
            CallbackCallerTools::onConnectFail("SendFriendMessageResult", SIGNAL(callbackSignal(QVariant, Fault, SendMessageResult)), callback.method);
        }
    }
};

}

#endif // SENDFRIENDMESSAGERESULT_H