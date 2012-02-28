#ifndef REMOVEFROMCONTACTSRESULT_H
#define REMOVEFROMCONTACTSRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "contactresultcode.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class RemoveFromContactsResultData : public QSharedData
{
public:
    RemoveFromContactsResultData();
    RemoveFromContactsResultData(ContactResultCode::ContactResultCodeEnum result);
    virtual ~RemoveFromContactsResultData();

    ContactResultCode::ContactResultCodeEnum result;
};

class RemoveFromContactsResult : public TransportableObject
{
public:
    RemoveFromContactsResult();
    RemoveFromContactsResult(ContactResultCode::ContactResultCodeEnum result);
    virtual ~RemoveFromContactsResult();

protected:
    RemoveFromContactsResult(RemoveFromContactsResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static RemoveFromContactsResult* ___new_()
    {
        return new RemoveFromContactsResult(new RemoveFromContactsResultData());
    }
    static RemoveFromContactsResult empty()
    {
        return RemoveFromContactsResult(new RemoveFromContactsResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    ContactResultCode::ContactResultCodeEnum getResult() const;
    void setResult(ContactResultCode::ContactResultCodeEnum value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<RemoveFromContactsResultData> d;
};

class RemoveFromContactsResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, ContactResultCode::ContactResultCodeEnum result)
    {
       RemoveFromContactsResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        RemoveFromContactsResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, ContactResultCode::Ok);
    }

signals:
    void callbackSignal(QVariant state, Fault fault, ContactResultCode::ContactResultCodeEnum result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, ContactResultCode::ContactResultCodeEnum result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, ContactResultCode::ContactResultCodeEnum)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("RemoveFromContactsResult", SIGNAL(callbackSignal(QVariant, Fault, ContactResultCode::ContactResultCodeEnum)), callback.method);
        }
    }
};

}

#endif // REMOVEFROMCONTACTSRESULT_H