#ifndef GETNOTIFICATIONTIMEOUTRESULT_H
#define GETNOTIFICATIONTIMEOUTRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class GetNotificationTimeoutResultData : public QSharedData
{
public:
    GetNotificationTimeoutResultData();
    GetNotificationTimeoutResultData(qint32 result);
    virtual ~GetNotificationTimeoutResultData();

    qint32 result;
};

class GetNotificationTimeoutResult : public TransportableObject
{
public:
    GetNotificationTimeoutResult();
    GetNotificationTimeoutResult(qint32 result);
    virtual ~GetNotificationTimeoutResult();

protected:
    GetNotificationTimeoutResult(GetNotificationTimeoutResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetNotificationTimeoutResult* ___new_()
    {
        return new GetNotificationTimeoutResult(new GetNotificationTimeoutResultData());
    }
    static GetNotificationTimeoutResult empty()
    {
        return GetNotificationTimeoutResult(new GetNotificationTimeoutResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    qint32 getResult() const;
    void setResult(qint32 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetNotificationTimeoutResultData> d;
};

class GetNotificationTimeoutResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, qint32 result)
    {
       GetNotificationTimeoutResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        GetNotificationTimeoutResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, 0);
    }

signals:
    void callbackSignal(QVariant state, Fault fault, qint32 result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, qint32 result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, qint32)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("GetNotificationTimeoutResult", SIGNAL(callbackSignal(QVariant, Fault, qint32)), callback.method);
        }
    }
};

}

#endif // GETNOTIFICATIONTIMEOUTRESULT_H