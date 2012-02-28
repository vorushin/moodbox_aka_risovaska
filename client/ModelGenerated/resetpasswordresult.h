#ifndef RESETPASSWORDRESULT_H
#define RESETPASSWORDRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "accountresultcode.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class ResetPasswordResultData : public QSharedData
{
public:
    ResetPasswordResultData();
    ResetPasswordResultData(AccountResultCode::AccountResultCodeEnum result);
    virtual ~ResetPasswordResultData();

    AccountResultCode::AccountResultCodeEnum result;
};

class ResetPasswordResult : public TransportableObject
{
public:
    ResetPasswordResult();
    ResetPasswordResult(AccountResultCode::AccountResultCodeEnum result);
    virtual ~ResetPasswordResult();

protected:
    ResetPasswordResult(ResetPasswordResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static ResetPasswordResult* ___new_()
    {
        return new ResetPasswordResult(new ResetPasswordResultData());
    }
    static ResetPasswordResult empty()
    {
        return ResetPasswordResult(new ResetPasswordResultData());
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
    QExplicitlySharedDataPointer<ResetPasswordResultData> d;
};

class ResetPasswordResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, AccountResultCode::AccountResultCodeEnum result)
    {
       ResetPasswordResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        ResetPasswordResultCallbackCaller caller;
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
            CallbackCallerTools::onConnectFail("ResetPasswordResult", SIGNAL(callbackSignal(QVariant, Fault, AccountResultCode::AccountResultCodeEnum)), callback.method);
        }
    }
};

}

#endif // RESETPASSWORDRESULT_H