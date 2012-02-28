#ifndef GETCHANNELINFORESULT_H
#define GETCHANNELINFORESULT_H

#include <QObject>
#include <QSharedData>

#include "channelresult.h"
#include "transportableobject.h"
#include "fault.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class GetChannelInfoResultData : public QSharedData
{
public:
    GetChannelInfoResultData();
    GetChannelInfoResultData(ChannelResult result);
    virtual ~GetChannelInfoResultData();

    ChannelResult result;
};

class GetChannelInfoResult : public TransportableObject
{
public:
    GetChannelInfoResult();
    GetChannelInfoResult(ChannelResult result);
    virtual ~GetChannelInfoResult();

protected:
    GetChannelInfoResult(GetChannelInfoResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetChannelInfoResult* ___new_()
    {
        return new GetChannelInfoResult(new GetChannelInfoResultData());
    }
    static GetChannelInfoResult empty()
    {
        return GetChannelInfoResult(new GetChannelInfoResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    ChannelResult getResult() const;
    void setResult(ChannelResult value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetChannelInfoResultData> d;
};

class GetChannelInfoResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, ChannelResult result)
    {
       GetChannelInfoResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        GetChannelInfoResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, ChannelResult());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, ChannelResult result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, ChannelResult result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, ChannelResult)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("GetChannelInfoResult", SIGNAL(callbackSignal(QVariant, Fault, ChannelResult)), callback.method);
        }
    }
};

}

#endif // GETCHANNELINFORESULT_H