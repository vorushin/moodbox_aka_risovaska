#ifndef DELETEMOODSTRIPRESULT_H
#define DELETEMOODSTRIPRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "moodstripresultcode.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class DeleteMoodstripResultData : public QSharedData
{
public:
    DeleteMoodstripResultData();
    DeleteMoodstripResultData(MoodstripResultCode::MoodstripResultCodeEnum result);
    virtual ~DeleteMoodstripResultData();

    MoodstripResultCode::MoodstripResultCodeEnum result;
};

class DeleteMoodstripResult : public TransportableObject
{
public:
    DeleteMoodstripResult();
    DeleteMoodstripResult(MoodstripResultCode::MoodstripResultCodeEnum result);
    virtual ~DeleteMoodstripResult();

protected:
    DeleteMoodstripResult(DeleteMoodstripResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static DeleteMoodstripResult* ___new_()
    {
        return new DeleteMoodstripResult(new DeleteMoodstripResultData());
    }
    static DeleteMoodstripResult empty()
    {
        return DeleteMoodstripResult(new DeleteMoodstripResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    MoodstripResultCode::MoodstripResultCodeEnum getResult() const;
    void setResult(MoodstripResultCode::MoodstripResultCodeEnum value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<DeleteMoodstripResultData> d;
};

class DeleteMoodstripResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, MoodstripResultCode::MoodstripResultCodeEnum result)
    {
       DeleteMoodstripResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        DeleteMoodstripResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, MoodstripResultCode::Ok);
    }

signals:
    void callbackSignal(QVariant state, Fault fault, MoodstripResultCode::MoodstripResultCodeEnum result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, MoodstripResultCode::MoodstripResultCodeEnum result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, MoodstripResultCode::MoodstripResultCodeEnum)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("DeleteMoodstripResult", SIGNAL(callbackSignal(QVariant, Fault, MoodstripResultCode::MoodstripResultCodeEnum)), callback.method);
        }
    }
};

}

#endif // DELETEMOODSTRIPRESULT_H