#ifndef PROCESSAUTHORIZATIONREQUESTRESULT_H
#define PROCESSAUTHORIZATIONREQUESTRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "authorizationresultcode.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class ProcessAuthorizationRequestResultData : public QSharedData
{
public:
    ProcessAuthorizationRequestResultData();
    ProcessAuthorizationRequestResultData(AuthorizationResultCode::AuthorizationResultCodeEnum result);
    virtual ~ProcessAuthorizationRequestResultData();

    AuthorizationResultCode::AuthorizationResultCodeEnum result;
};

class ProcessAuthorizationRequestResult : public TransportableObject
{
public:
    ProcessAuthorizationRequestResult();
    ProcessAuthorizationRequestResult(AuthorizationResultCode::AuthorizationResultCodeEnum result);
    virtual ~ProcessAuthorizationRequestResult();

protected:
    ProcessAuthorizationRequestResult(ProcessAuthorizationRequestResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static ProcessAuthorizationRequestResult* ___new_()
    {
        return new ProcessAuthorizationRequestResult(new ProcessAuthorizationRequestResultData());
    }
    static ProcessAuthorizationRequestResult empty()
    {
        return ProcessAuthorizationRequestResult(new ProcessAuthorizationRequestResultData());
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
    QExplicitlySharedDataPointer<ProcessAuthorizationRequestResultData> d;
};

class ProcessAuthorizationRequestResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, AuthorizationResultCode::AuthorizationResultCodeEnum result)
    {
       ProcessAuthorizationRequestResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        ProcessAuthorizationRequestResultCallbackCaller caller;
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
            CallbackCallerTools::onConnectFail("ProcessAuthorizationRequestResult", SIGNAL(callbackSignal(QVariant, Fault, AuthorizationResultCode::AuthorizationResultCodeEnum)), callback.method);
        }
    }
};

}

#endif // PROCESSAUTHORIZATIONREQUESTRESULT_H