#ifndef GETUSERINFOBYLOGINRESULT_H
#define GETUSERINFOBYLOGINRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "userinfo.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class GetUserInfoByLoginResultData : public QSharedData
{
public:
    GetUserInfoByLoginResultData();
    GetUserInfoByLoginResultData(UserInfo result);
    virtual ~GetUserInfoByLoginResultData();

    UserInfo result;
};

class GetUserInfoByLoginResult : public TransportableObject
{
public:
    GetUserInfoByLoginResult();
    GetUserInfoByLoginResult(UserInfo result);
    virtual ~GetUserInfoByLoginResult();

protected:
    GetUserInfoByLoginResult(GetUserInfoByLoginResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetUserInfoByLoginResult* ___new_()
    {
        return new GetUserInfoByLoginResult(new GetUserInfoByLoginResultData());
    }
    static GetUserInfoByLoginResult empty()
    {
        return GetUserInfoByLoginResult(new GetUserInfoByLoginResultData());
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
    QExplicitlySharedDataPointer<GetUserInfoByLoginResultData> d;
};

class GetUserInfoByLoginResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, UserInfo result)
    {
       GetUserInfoByLoginResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        GetUserInfoByLoginResultCallbackCaller caller;
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
            CallbackCallerTools::onConnectFail("GetUserInfoByLoginResult", SIGNAL(callbackSignal(QVariant, Fault, UserInfo)), callback.method);
        }
    }
};

}

#endif // GETUSERINFOBYLOGINRESULT_H