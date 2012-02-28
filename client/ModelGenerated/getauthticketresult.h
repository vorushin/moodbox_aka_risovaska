#ifndef GETAUTHTICKETRESULT_H
#define GETAUTHTICKETRESULT_H

#include <QObject>
#include <QSharedData>

#include "authticketresult.h"
#include "transportableobject.h"
#include "fault.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class GetAuthTicketResultData : public QSharedData
{
public:
    GetAuthTicketResultData();
    GetAuthTicketResultData(AuthTicketResult result);
    virtual ~GetAuthTicketResultData();

    AuthTicketResult result;
};

class GetAuthTicketResult : public TransportableObject
{
public:
    GetAuthTicketResult();
    GetAuthTicketResult(AuthTicketResult result);
    virtual ~GetAuthTicketResult();

protected:
    GetAuthTicketResult(GetAuthTicketResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetAuthTicketResult* ___new_()
    {
        return new GetAuthTicketResult(new GetAuthTicketResultData());
    }
    static GetAuthTicketResult empty()
    {
        return GetAuthTicketResult(new GetAuthTicketResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    AuthTicketResult getResult() const;
    void setResult(AuthTicketResult value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetAuthTicketResultData> d;
};

class GetAuthTicketResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, AuthTicketResult result)
    {
       GetAuthTicketResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        GetAuthTicketResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, AuthTicketResult());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, AuthTicketResult result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, AuthTicketResult result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, AuthTicketResult)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("GetAuthTicketResult", SIGNAL(callbackSignal(QVariant, Fault, AuthTicketResult)), callback.method);
        }
    }
};

}

#endif // GETAUTHTICKETRESULT_H