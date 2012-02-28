#ifndef NOTIFICATIONREGISTERRESULT_H
#define NOTIFICATIONREGISTERRESULT_H

#include <QObject>
#include <QSharedData>

#include "notificationregistrationresult.h"
#include "transportableobject.h"
#include "fault.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class NotificationRegisterResultData : public QSharedData
{
public:
    NotificationRegisterResultData();
    NotificationRegisterResultData(NotificationRegistrationResult result);
    virtual ~NotificationRegisterResultData();

    NotificationRegistrationResult result;
};

class NotificationRegisterResult : public TransportableObject
{
public:
    NotificationRegisterResult();
    NotificationRegisterResult(NotificationRegistrationResult result);
    virtual ~NotificationRegisterResult();

protected:
    NotificationRegisterResult(NotificationRegisterResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static NotificationRegisterResult* ___new_()
    {
        return new NotificationRegisterResult(new NotificationRegisterResultData());
    }
    static NotificationRegisterResult empty()
    {
        return NotificationRegisterResult(new NotificationRegisterResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    NotificationRegistrationResult getResult() const;
    void setResult(NotificationRegistrationResult value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<NotificationRegisterResultData> d;
};

class NotificationRegisterResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, NotificationRegistrationResult result)
    {
       NotificationRegisterResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        NotificationRegisterResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, NotificationRegistrationResult());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, NotificationRegistrationResult result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, NotificationRegistrationResult result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, NotificationRegistrationResult)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("NotificationRegisterResult", SIGNAL(callbackSignal(QVariant, Fault, NotificationRegistrationResult)), callback.method);
        }
    }
};

}

#endif // NOTIFICATIONREGISTERRESULT_H