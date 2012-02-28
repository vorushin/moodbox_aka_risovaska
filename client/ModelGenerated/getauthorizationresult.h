#ifndef GETAUTHORIZATIONRESULT_H
#define GETAUTHORIZATIONRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "authorization.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class GetAuthorizationResultData : public QSharedData
{
public:
    GetAuthorizationResultData();
    GetAuthorizationResultData(Authorization result);
    virtual ~GetAuthorizationResultData();

    Authorization result;
};

class GetAuthorizationResult : public TransportableObject
{
public:
    GetAuthorizationResult();
    GetAuthorizationResult(Authorization result);
    virtual ~GetAuthorizationResult();

protected:
    GetAuthorizationResult(GetAuthorizationResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetAuthorizationResult* ___new_()
    {
        return new GetAuthorizationResult(new GetAuthorizationResultData());
    }
    static GetAuthorizationResult empty()
    {
        return GetAuthorizationResult(new GetAuthorizationResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    Authorization getResult() const;
    void setResult(Authorization value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetAuthorizationResultData> d;
};

class GetAuthorizationResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, Authorization result)
    {
       GetAuthorizationResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        GetAuthorizationResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, Authorization());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, Authorization result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, Authorization result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, Authorization)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("GetAuthorizationResult", SIGNAL(callbackSignal(QVariant, Fault, Authorization)), callback.method);
        }
    }
};

}

#endif // GETAUTHORIZATIONRESULT_H