#ifndef OBSCENECHANNELMESSAGERESULT_H
#define OBSCENECHANNELMESSAGERESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "standartresultcode.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class ObsceneChannelMessageResultData : public QSharedData
{
public:
    ObsceneChannelMessageResultData();
    ObsceneChannelMessageResultData(StandartResultCode::StandartResultCodeEnum result);
    virtual ~ObsceneChannelMessageResultData();

    StandartResultCode::StandartResultCodeEnum result;
};

class ObsceneChannelMessageResult : public TransportableObject
{
public:
    ObsceneChannelMessageResult();
    ObsceneChannelMessageResult(StandartResultCode::StandartResultCodeEnum result);
    virtual ~ObsceneChannelMessageResult();

protected:
    ObsceneChannelMessageResult(ObsceneChannelMessageResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static ObsceneChannelMessageResult* ___new_()
    {
        return new ObsceneChannelMessageResult(new ObsceneChannelMessageResultData());
    }
    static ObsceneChannelMessageResult empty()
    {
        return ObsceneChannelMessageResult(new ObsceneChannelMessageResultData());
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
    QExplicitlySharedDataPointer<ObsceneChannelMessageResultData> d;
};

class ObsceneChannelMessageResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, StandartResultCode::StandartResultCodeEnum result)
    {
       ObsceneChannelMessageResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        ObsceneChannelMessageResultCallbackCaller caller;
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
            CallbackCallerTools::onConnectFail("ObsceneChannelMessageResult", SIGNAL(callbackSignal(QVariant, Fault, StandartResultCode::StandartResultCodeEnum)), callback.method);
        }
    }
};

}

#endif // OBSCENECHANNELMESSAGERESULT_H