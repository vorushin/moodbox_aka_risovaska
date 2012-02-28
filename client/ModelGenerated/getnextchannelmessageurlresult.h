#ifndef GETNEXTCHANNELMESSAGEURLRESULT_H
#define GETNEXTCHANNELMESSAGEURLRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "channelmessageurl.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class GetNextChannelMessageUrlResultData : public QSharedData
{
public:
    GetNextChannelMessageUrlResultData();
    GetNextChannelMessageUrlResultData(ChannelMessageUrl result);
    virtual ~GetNextChannelMessageUrlResultData();

    ChannelMessageUrl result;
};

class GetNextChannelMessageUrlResult : public TransportableObject
{
public:
    GetNextChannelMessageUrlResult();
    GetNextChannelMessageUrlResult(ChannelMessageUrl result);
    virtual ~GetNextChannelMessageUrlResult();

protected:
    GetNextChannelMessageUrlResult(GetNextChannelMessageUrlResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetNextChannelMessageUrlResult* ___new_()
    {
        return new GetNextChannelMessageUrlResult(new GetNextChannelMessageUrlResultData());
    }
    static GetNextChannelMessageUrlResult empty()
    {
        return GetNextChannelMessageUrlResult(new GetNextChannelMessageUrlResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    ChannelMessageUrl getResult() const;
    void setResult(ChannelMessageUrl value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetNextChannelMessageUrlResultData> d;
};

class GetNextChannelMessageUrlResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, ChannelMessageUrl result)
    {
       GetNextChannelMessageUrlResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        GetNextChannelMessageUrlResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, ChannelMessageUrl());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, ChannelMessageUrl result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, ChannelMessageUrl result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, ChannelMessageUrl)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("GetNextChannelMessageUrlResult", SIGNAL(callbackSignal(QVariant, Fault, ChannelMessageUrl)), callback.method);
        }
    }
};

}

#endif // GETNEXTCHANNELMESSAGEURLRESULT_H