#ifndef GETNEXTCHANNELMESSAGERESULT_H
#define GETNEXTCHANNELMESSAGERESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "channelmessage.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class GetNextChannelMessageResultData : public QSharedData
{
public:
    GetNextChannelMessageResultData();
    GetNextChannelMessageResultData(ChannelMessage result);
    virtual ~GetNextChannelMessageResultData();

    ChannelMessage result;
};

class GetNextChannelMessageResult : public TransportableObject
{
public:
    GetNextChannelMessageResult();
    GetNextChannelMessageResult(ChannelMessage result);
    virtual ~GetNextChannelMessageResult();

protected:
    GetNextChannelMessageResult(GetNextChannelMessageResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetNextChannelMessageResult* ___new_()
    {
        return new GetNextChannelMessageResult(new GetNextChannelMessageResultData());
    }
    static GetNextChannelMessageResult empty()
    {
        return GetNextChannelMessageResult(new GetNextChannelMessageResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    ChannelMessage getResult() const;
    void setResult(ChannelMessage value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetNextChannelMessageResultData> d;
};

class GetNextChannelMessageResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, ChannelMessage result)
    {
       GetNextChannelMessageResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        GetNextChannelMessageResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, ChannelMessage());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, ChannelMessage result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, ChannelMessage result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, ChannelMessage)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("GetNextChannelMessageResult", SIGNAL(callbackSignal(QVariant, Fault, ChannelMessage)), callback.method);
        }
    }
};

}

#endif // GETNEXTCHANNELMESSAGERESULT_H