#ifndef GETMYACCOUNTRESULT_H
#define GETMYACCOUNTRESULT_H

#include <QObject>
#include <QSharedData>

#include "useraccount.h"
#include "transportableobject.h"
#include "fault.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class GetMyAccountResultData : public QSharedData
{
public:
    GetMyAccountResultData();
    GetMyAccountResultData(UserAccount result);
    virtual ~GetMyAccountResultData();

    UserAccount result;
};

class GetMyAccountResult : public TransportableObject
{
public:
    GetMyAccountResult();
    GetMyAccountResult(UserAccount result);
    virtual ~GetMyAccountResult();

protected:
    GetMyAccountResult(GetMyAccountResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetMyAccountResult* ___new_()
    {
        return new GetMyAccountResult(new GetMyAccountResultData());
    }
    static GetMyAccountResult empty()
    {
        return GetMyAccountResult(new GetMyAccountResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    UserAccount getResult() const;
    void setResult(UserAccount value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetMyAccountResultData> d;
};

class GetMyAccountResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, UserAccount result)
    {
       GetMyAccountResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        GetMyAccountResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, UserAccount());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, UserAccount result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, UserAccount result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, UserAccount)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("GetMyAccountResult", SIGNAL(callbackSignal(QVariant, Fault, UserAccount)), callback.method);
        }
    }
};

}

#endif // GETMYACCOUNTRESULT_H