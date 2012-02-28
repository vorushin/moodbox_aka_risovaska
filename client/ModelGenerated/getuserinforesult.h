#ifndef GETUSERINFORESULT_H
#define GETUSERINFORESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "userinfo.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class GetUserInfoResultData : public QSharedData
{
public:
    GetUserInfoResultData();
    GetUserInfoResultData(UserInfo result);
    virtual ~GetUserInfoResultData();

    UserInfo result;
};

class GetUserInfoResult : public TransportableObject
{
public:
    GetUserInfoResult();
    GetUserInfoResult(UserInfo result);
    virtual ~GetUserInfoResult();

protected:
    GetUserInfoResult(GetUserInfoResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetUserInfoResult* ___new_()
    {
        return new GetUserInfoResult(new GetUserInfoResultData());
    }
    static GetUserInfoResult empty()
    {
        return GetUserInfoResult(new GetUserInfoResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    UserInfo getResult() const;
    void setResult(UserInfo value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetUserInfoResultData> d;
};

class GetUserInfoResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, UserInfo result)
    {
       GetUserInfoResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        GetUserInfoResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, UserInfo());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, UserInfo result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, UserInfo result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, UserInfo)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("GetUserInfoResult", SIGNAL(callbackSignal(QVariant, Fault, UserInfo)), callback.method);
        }
    }
};

}

#endif // GETUSERINFORESULT_H