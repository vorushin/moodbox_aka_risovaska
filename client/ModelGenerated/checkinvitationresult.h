#ifndef CHECKINVITATIONRESULT_H
#define CHECKINVITATIONRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "invitationresultcode.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class CheckInvitationResultData : public QSharedData
{
public:
    CheckInvitationResultData();
    CheckInvitationResultData(InvitationResultCode::InvitationResultCodeEnum result);
    virtual ~CheckInvitationResultData();

    InvitationResultCode::InvitationResultCodeEnum result;
};

class CheckInvitationResult : public TransportableObject
{
public:
    CheckInvitationResult();
    CheckInvitationResult(InvitationResultCode::InvitationResultCodeEnum result);
    virtual ~CheckInvitationResult();

protected:
    CheckInvitationResult(CheckInvitationResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static CheckInvitationResult* ___new_()
    {
        return new CheckInvitationResult(new CheckInvitationResultData());
    }
    static CheckInvitationResult empty()
    {
        return CheckInvitationResult(new CheckInvitationResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    InvitationResultCode::InvitationResultCodeEnum getResult() const;
    void setResult(InvitationResultCode::InvitationResultCodeEnum value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<CheckInvitationResultData> d;
};

class CheckInvitationResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, InvitationResultCode::InvitationResultCodeEnum result)
    {
       CheckInvitationResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        CheckInvitationResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, InvitationResultCode::Ok);
    }

signals:
    void callbackSignal(QVariant state, Fault fault, InvitationResultCode::InvitationResultCodeEnum result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, InvitationResultCode::InvitationResultCodeEnum result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, InvitationResultCode::InvitationResultCodeEnum)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("CheckInvitationResult", SIGNAL(callbackSignal(QVariant, Fault, InvitationResultCode::InvitationResultCodeEnum)), callback.method);
        }
    }
};

}

#endif // CHECKINVITATIONRESULT_H