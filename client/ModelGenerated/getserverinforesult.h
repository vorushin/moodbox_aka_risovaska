#ifndef GETSERVERINFORESULT_H
#define GETSERVERINFORESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "serverinfo.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class GetServerInfoResultData : public QSharedData
{
public:
    GetServerInfoResultData();
    GetServerInfoResultData(ServerInfo result);
    virtual ~GetServerInfoResultData();

    ServerInfo result;
};

class GetServerInfoResult : public TransportableObject
{
public:
    GetServerInfoResult();
    GetServerInfoResult(ServerInfo result);
    virtual ~GetServerInfoResult();

protected:
    GetServerInfoResult(GetServerInfoResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetServerInfoResult* ___new_()
    {
        return new GetServerInfoResult(new GetServerInfoResultData());
    }
    static GetServerInfoResult empty()
    {
        return GetServerInfoResult(new GetServerInfoResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    ServerInfo getResult() const;
    void setResult(ServerInfo value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetServerInfoResultData> d;
};

class GetServerInfoResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, ServerInfo result)
    {
       GetServerInfoResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        GetServerInfoResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, ServerInfo());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, ServerInfo result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, ServerInfo result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, ServerInfo)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("GetServerInfoResult", SIGNAL(callbackSignal(QVariant, Fault, ServerInfo)), callback.method);
        }
    }
};

}

#endif // GETSERVERINFORESULT_H