#ifndef DELETEMESSAGERESULT_H
#define DELETEMESSAGERESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "standartresultcode.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class DeleteMessageResultData : public QSharedData
{
public:
    DeleteMessageResultData();
    DeleteMessageResultData(StandartResultCode::StandartResultCodeEnum result);
    virtual ~DeleteMessageResultData();

    StandartResultCode::StandartResultCodeEnum result;
};

class DeleteMessageResult : public TransportableObject
{
public:
    DeleteMessageResult();
    DeleteMessageResult(StandartResultCode::StandartResultCodeEnum result);
    virtual ~DeleteMessageResult();

protected:
    DeleteMessageResult(DeleteMessageResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static DeleteMessageResult* ___new_()
    {
        return new DeleteMessageResult(new DeleteMessageResultData());
    }
    static DeleteMessageResult empty()
    {
        return DeleteMessageResult(new DeleteMessageResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    StandartResultCode::StandartResultCodeEnum getResult() const;
    void setResult(StandartResultCode::StandartResultCodeEnum value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<DeleteMessageResultData> d;
};

class DeleteMessageResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, StandartResultCode::StandartResultCodeEnum result)
    {
       DeleteMessageResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        DeleteMessageResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, StandartResultCode::Undefined);
    }

signals:
    void callbackSignal(QVariant state, Fault fault, StandartResultCode::StandartResultCodeEnum result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, StandartResultCode::StandartResultCodeEnum result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, StandartResultCode::StandartResultCodeEnum)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("DeleteMessageResult", SIGNAL(callbackSignal(QVariant, Fault, StandartResultCode::StandartResultCodeEnum)), callback.method);
        }
    }
};

}

#endif // DELETEMESSAGERESULT_H