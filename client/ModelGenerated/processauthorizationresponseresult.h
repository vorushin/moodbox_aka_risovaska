#ifndef PROCESSAUTHORIZATIONRESPONSERESULT_H
#define PROCESSAUTHORIZATIONRESPONSERESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "authorizationresultcode.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class ProcessAuthorizationResponseResultData : public QSharedData
{
public:
    ProcessAuthorizationResponseResultData();
    ProcessAuthorizationResponseResultData(AuthorizationResultCode::AuthorizationResultCodeEnum result);
    virtual ~ProcessAuthorizationResponseResultData();

    AuthorizationResultCode::AuthorizationResultCodeEnum result;
};

class ProcessAuthorizationResponseResult : public TransportableObject
{
public:
    ProcessAuthorizationResponseResult();
    ProcessAuthorizationResponseResult(AuthorizationResultCode::AuthorizationResultCodeEnum result);
    virtual ~ProcessAuthorizationResponseResult();

protected:
    ProcessAuthorizationResponseResult(ProcessAuthorizationResponseResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static ProcessAuthorizationResponseResult* ___new_()
    {
        return new ProcessAuthorizationResponseResult(new ProcessAuthorizationResponseResultData());
    }
    static ProcessAuthorizationResponseResult empty()
    {
        return ProcessAuthorizationResponseResult(new ProcessAuthorizationResponseResultData());
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
    QExplicitlySharedDataPointer<ProcessAuthorizationResponseResultData> d;
};

class ProcessAuthorizationResponseResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, AuthorizationResultCode::AuthorizationResultCodeEnum result)
    {
       ProcessAuthorizationResponseResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        ProcessAuthorizationResponseResultCallbackCaller caller;
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
            CallbackCallerTools::onConnectFail("ProcessAuthorizationResponseResult", SIGNAL(callbackSignal(QVariant, Fault, AuthorizationResultCode::AuthorizationResultCodeEnum)), callback.method);
        }
    }
};

}

#endif // PROCESSAUTHORIZATIONRESPONSERESULT_H