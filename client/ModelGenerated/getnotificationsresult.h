#ifndef GETNOTIFICATIONSRESULT_H
#define GETNOTIFICATIONSRESULT_H

#include <QObject>
#include <QSharedData>

#include "notificationresult.h"
#include "transportableobject.h"
#include "fault.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class GetNotificationsResultData : public QSharedData
{
public:
    GetNotificationsResultData();
    GetNotificationsResultData(NotificationResult result);
    virtual ~GetNotificationsResultData();

    NotificationResult result;
};

class GetNotificationsResult : public TransportableObject
{
public:
    GetNotificationsResult();
    GetNotificationsResult(NotificationResult result);
    virtual ~GetNotificationsResult();

protected:
    GetNotificationsResult(GetNotificationsResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetNotificationsResult* ___new_()
    {
        return new GetNotificationsResult(new GetNotificationsResultData());
    }
    static GetNotificationsResult empty()
    {
        return GetNotificationsResult(new GetNotificationsResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    NotificationResult getResult() const;
    void setResult(NotificationResult value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetNotificationsResultData> d;
};

class GetNotificationsResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, NotificationResult result)
    {
       GetNotificationsResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        GetNotificationsResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, NotificationResult());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, NotificationResult result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, NotificationResult result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, NotificationResult)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("GetNotificationsResult", SIGNAL(callbackSignal(QVariant, Fault, NotificationResult)), callback.method);
        }
    }
};

}

#endif // GETNOTIFICATIONSRESULT_H