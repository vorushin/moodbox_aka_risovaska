#ifndef UPDATEPASSWORDRESULT_H
#define UPDATEPASSWORDRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "accountresultcode.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class UpdatePasswordResultData : public QSharedData
{
public:
    UpdatePasswordResultData();
    UpdatePasswordResultData(AccountResultCode::AccountResultCodeEnum result);
    virtual ~UpdatePasswordResultData();

    AccountResultCode::AccountResultCodeEnum result;
};

class UpdatePasswordResult : public TransportableObject
{
public:
    UpdatePasswordResult();
    UpdatePasswordResult(AccountResultCode::AccountResultCodeEnum result);
    virtual ~UpdatePasswordResult();

protected:
    UpdatePasswordResult(UpdatePasswordResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static UpdatePasswordResult* ___new_()
    {
        return new UpdatePasswordResult(new UpdatePasswordResultData());
    }
    static UpdatePasswordResult empty()
    {
        return UpdatePasswordResult(new UpdatePasswordResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    AccountResultCode::AccountResultCodeEnum getResult() const;
    void setResult(AccountResultCode::AccountResultCodeEnum value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<UpdatePasswordResultData> d;
};

class UpdatePasswordResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, AccountResultCode::AccountResultCodeEnum result)
    {
       UpdatePasswordResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        UpdatePasswordResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, AccountResultCode::Ok);
    }

signals:
    void callbackSignal(QVariant state, Fault fault, AccountResultCode::AccountResultCodeEnum result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, AccountResultCode::AccountResultCodeEnum result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, AccountResultCode::AccountResultCodeEnum)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("UpdatePasswordResult", SIGNAL(callbackSignal(QVariant, Fault, AccountResultCode::AccountResultCodeEnum)), callback.method);
        }
    }
};

}

#endif // UPDATEPASSWORDRESULT_H