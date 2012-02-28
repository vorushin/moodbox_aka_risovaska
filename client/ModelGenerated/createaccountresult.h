#ifndef CREATEACCOUNTRESULT_H
#define CREATEACCOUNTRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "accountresultcode.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class CreateAccountResultData : public QSharedData
{
public:
    CreateAccountResultData();
    CreateAccountResultData(AccountResultCode::AccountResultCodeEnum result);
    virtual ~CreateAccountResultData();

    AccountResultCode::AccountResultCodeEnum result;
};

class CreateAccountResult : public TransportableObject
{
public:
    CreateAccountResult();
    CreateAccountResult(AccountResultCode::AccountResultCodeEnum result);
    virtual ~CreateAccountResult();

protected:
    CreateAccountResult(CreateAccountResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static CreateAccountResult* ___new_()
    {
        return new CreateAccountResult(new CreateAccountResultData());
    }
    static CreateAccountResult empty()
    {
        return CreateAccountResult(new CreateAccountResultData());
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
    QExplicitlySharedDataPointer<CreateAccountResultData> d;
};

class CreateAccountResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, AccountResultCode::AccountResultCodeEnum result)
    {
       CreateAccountResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        CreateAccountResultCallbackCaller caller;
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
            CallbackCallerTools::onConnectFail("CreateAccountResult", SIGNAL(callbackSignal(QVariant, Fault, AccountResultCode::AccountResultCodeEnum)), callback.method);
        }
    }
};

}

#endif // CREATEACCOUNTRESULT_H