#ifndef UPDATEACCOUNTRESULT_H
#define UPDATEACCOUNTRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "accountresultcode.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class UpdateAccountResultData : public QSharedData
{
public:
    UpdateAccountResultData();
    UpdateAccountResultData(AccountResultCode::AccountResultCodeEnum result);
    virtual ~UpdateAccountResultData();

    AccountResultCode::AccountResultCodeEnum result;
};

class UpdateAccountResult : public TransportableObject
{
public:
    UpdateAccountResult();
    UpdateAccountResult(AccountResultCode::AccountResultCodeEnum result);
    virtual ~UpdateAccountResult();

protected:
    UpdateAccountResult(UpdateAccountResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static UpdateAccountResult* ___new_()
    {
        return new UpdateAccountResult(new UpdateAccountResultData());
    }
    static UpdateAccountResult empty()
    {
        return UpdateAccountResult(new UpdateAccountResultData());
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
    QExplicitlySharedDataPointer<UpdateAccountResultData> d;
};

class UpdateAccountResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, AccountResultCode::AccountResultCodeEnum result)
    {
       UpdateAccountResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        UpdateAccountResultCallbackCaller caller;
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
            CallbackCallerTools::onConnectFail("UpdateAccountResult", SIGNAL(callbackSignal(QVariant, Fault, AccountResultCode::AccountResultCodeEnum)), callback.method);
        }
    }
};

}

#endif // UPDATEACCOUNTRESULT_H