#ifndef GETUSERPICTURERESULT_H
#define GETUSERPICTURERESULT_H

#include <QObject>
#include <QSharedData>

#include "userpictureresult.h"
#include "transportableobject.h"
#include "fault.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class GetUserPictureResultData : public QSharedData
{
public:
    GetUserPictureResultData();
    GetUserPictureResultData(UserPictureResult result);
    virtual ~GetUserPictureResultData();

    UserPictureResult result;
};

class GetUserPictureResult : public TransportableObject
{
public:
    GetUserPictureResult();
    GetUserPictureResult(UserPictureResult result);
    virtual ~GetUserPictureResult();

protected:
    GetUserPictureResult(GetUserPictureResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetUserPictureResult* ___new_()
    {
        return new GetUserPictureResult(new GetUserPictureResultData());
    }
    static GetUserPictureResult empty()
    {
        return GetUserPictureResult(new GetUserPictureResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    UserPictureResult getResult() const;
    void setResult(UserPictureResult value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetUserPictureResultData> d;
};

class GetUserPictureResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, UserPictureResult result)
    {
       GetUserPictureResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        GetUserPictureResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, UserPictureResult());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, UserPictureResult result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, UserPictureResult result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, UserPictureResult)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("GetUserPictureResult", SIGNAL(callbackSignal(QVariant, Fault, UserPictureResult)), callback.method);
        }
    }
};

}

#endif // GETUSERPICTURERESULT_H