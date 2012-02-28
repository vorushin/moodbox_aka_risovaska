#ifndef NOTIFICATIONUNREGISTERRESULT_H
#define NOTIFICATIONUNREGISTERRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "okresultcode.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class NotificationUnregisterResultData : public QSharedData
{
public:
    NotificationUnregisterResultData();
    NotificationUnregisterResultData(OkResultCode::OkResultCodeEnum result);
    virtual ~NotificationUnregisterResultData();

    OkResultCode::OkResultCodeEnum result;
};

class NotificationUnregisterResult : public TransportableObject
{
public:
    NotificationUnregisterResult();
    NotificationUnregisterResult(OkResultCode::OkResultCodeEnum result);
    virtual ~NotificationUnregisterResult();

protected:
    NotificationUnregisterResult(NotificationUnregisterResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static NotificationUnregisterResult* ___new_()
    {
        return new NotificationUnregisterResult(new NotificationUnregisterResultData());
    }
    static NotificationUnregisterResult empty()
    {
        return NotificationUnregisterResult(new NotificationUnregisterResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    OkResultCode::OkResultCodeEnum getResult() const;
    void setResult(OkResultCode::OkResultCodeEnum value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<NotificationUnregisterResultData> d;
};

class NotificationUnregisterResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, OkResultCode::OkResultCodeEnum result)
    {
       NotificationUnregisterResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        NotificationUnregisterResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, OkResultCode::Undefined);
    }

signals:
    void callbackSignal(QVariant state, Fault fault, OkResultCode::OkResultCodeEnum result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, OkResultCode::OkResultCodeEnum result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, OkResultCode::OkResultCodeEnum)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("NotificationUnregisterResult", SIGNAL(callbackSignal(QVariant, Fault, OkResultCode::OkResultCodeEnum)), callback.method);
        }
    }
};

}

#endif // NOTIFICATIONUNREGISTERRESULT_H