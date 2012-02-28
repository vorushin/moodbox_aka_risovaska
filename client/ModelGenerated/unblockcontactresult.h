#ifndef UNBLOCKCONTACTRESULT_H
#define UNBLOCKCONTACTRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "contactresultcode.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class UnblockContactResultData : public QSharedData
{
public:
    UnblockContactResultData();
    UnblockContactResultData(ContactResultCode::ContactResultCodeEnum result);
    virtual ~UnblockContactResultData();

    ContactResultCode::ContactResultCodeEnum result;
};

class UnblockContactResult : public TransportableObject
{
public:
    UnblockContactResult();
    UnblockContactResult(ContactResultCode::ContactResultCodeEnum result);
    virtual ~UnblockContactResult();

protected:
    UnblockContactResult(UnblockContactResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static UnblockContactResult* ___new_()
    {
        return new UnblockContactResult(new UnblockContactResultData());
    }
    static UnblockContactResult empty()
    {
        return UnblockContactResult(new UnblockContactResultData());
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
    QExplicitlySharedDataPointer<UnblockContactResultData> d;
};

class UnblockContactResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, ContactResultCode::ContactResultCodeEnum result)
    {
       UnblockContactResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        UnblockContactResultCallbackCaller caller;
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
            CallbackCallerTools::onConnectFail("UnblockContactResult", SIGNAL(callbackSignal(QVariant, Fault, ContactResultCode::ContactResultCodeEnum)), callback.method);
        }
    }
};

}

#endif // UNBLOCKCONTACTRESULT_H