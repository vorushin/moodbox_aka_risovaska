#ifndef PROCESSAUTHORIZATIONREQUESTBYLOGINRESULT_H
#define PROCESSAUTHORIZATIONREQUESTBYLOGINRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "authorizationresultcode.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class ProcessAuthorizationRequestByLoginResultData : public QSharedData
{
public:
    ProcessAuthorizationRequestByLoginResultData();
    ProcessAuthorizationRequestByLoginResultData(AuthorizationResultCode::AuthorizationResultCodeEnum result);
    virtual ~ProcessAuthorizationRequestByLoginResultData();

    AuthorizationResultCode::AuthorizationResultCodeEnum result;
};

class ProcessAuthorizationRequestByLoginResult : public TransportableObject
{
public:
    ProcessAuthorizationRequestByLoginResult();
    ProcessAuthorizationRequestByLoginResult(AuthorizationResultCode::AuthorizationResultCodeEnum result);
    virtual ~ProcessAuthorizationRequestByLoginResult();

protected:
    ProcessAuthorizationRequestByLoginResult(ProcessAuthorizationRequestByLoginResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static ProcessAuthorizationRequestByLoginResult* ___new_()
    {
        return new ProcessAuthorizationRequestByLoginResult(new ProcessAuthorizationRequestByLoginResultData());
    }
    static ProcessAuthorizationRequestByLoginResult empty()
    {
        return ProcessAuthorizationRequestByLoginResult(new ProcessAuthorizationRequestByLoginResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    AuthorizationResultCode::AuthorizationResultCodeEnum getResult() const;
    void setResult(AuthorizationResultCode::AuthorizationResultCodeEnum value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<ProcessAuthorizationRequestByLoginResultData> d;
};

class ProcessAuthorizationRequestByLoginResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, AuthorizationResultCode::AuthorizationResultCodeEnum result)
    {
       ProcessAuthorizationRequestByLoginResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        ProcessAuthorizationRequestByLoginResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, AuthorizationResultCode::Ok);
    }

signals:
    void callbackSignal(QVariant state, Fault fault, AuthorizationResultCode::AuthorizationResultCodeEnum result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, AuthorizationResultCode::AuthorizationResultCodeEnum result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, AuthorizationResultCode::AuthorizationResultCodeEnum)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("ProcessAuthorizationRequestByLoginResult", SIGNAL(callbackSignal(QVariant, Fault, AuthorizationResultCode::AuthorizationResultCodeEnum)), callback.method);
        }
    }
};

}

#endif // PROCESSAUTHORIZATIONREQUESTBYLOGINRESULT_H