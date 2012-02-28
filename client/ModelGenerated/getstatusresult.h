#ifndef GETSTATUSRESULT_H
#define GETSTATUSRESULT_H

#include <QObject>
#include <QSharedData>

#include "userstatus.h"
#include "transportableobject.h"
#include "fault.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class GetStatusResultData : public QSharedData
{
public:
    GetStatusResultData();
    GetStatusResultData(UserStatus::UserStatusEnum result);
    virtual ~GetStatusResultData();

    UserStatus::UserStatusEnum result;
};

class GetStatusResult : public TransportableObject
{
public:
    GetStatusResult();
    GetStatusResult(UserStatus::UserStatusEnum result);
    virtual ~GetStatusResult();

protected:
    GetStatusResult(GetStatusResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetStatusResult* ___new_()
    {
        return new GetStatusResult(new GetStatusResultData());
    }
    static GetStatusResult empty()
    {
        return GetStatusResult(new GetStatusResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    UserStatus::UserStatusEnum getResult() const;
    void setResult(UserStatus::UserStatusEnum value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetStatusResultData> d;
};

class GetStatusResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, UserStatus::UserStatusEnum result)
    {
       GetStatusResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        GetStatusResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, UserStatus::Undefined);
    }

signals:
    void callbackSignal(QVariant state, Fault fault, UserStatus::UserStatusEnum result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, UserStatus::UserStatusEnum result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, UserStatus::UserStatusEnum)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("GetStatusResult", SIGNAL(callbackSignal(QVariant, Fault, UserStatus::UserStatusEnum)), callback.method);
        }
    }
};

}

#endif // GETSTATUSRESULT_H