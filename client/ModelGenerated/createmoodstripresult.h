#ifndef CREATEMOODSTRIPRESULT_H
#define CREATEMOODSTRIPRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class CreateMoodstripResultData : public QSharedData
{
public:
    CreateMoodstripResultData();
    CreateMoodstripResultData(qint32 result);
    virtual ~CreateMoodstripResultData();

    qint32 result;
};

class CreateMoodstripResult : public TransportableObject
{
public:
    CreateMoodstripResult();
    CreateMoodstripResult(qint32 result);
    virtual ~CreateMoodstripResult();

protected:
    CreateMoodstripResult(CreateMoodstripResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static CreateMoodstripResult* ___new_()
    {
        return new CreateMoodstripResult(new CreateMoodstripResultData());
    }
    static CreateMoodstripResult empty()
    {
        return CreateMoodstripResult(new CreateMoodstripResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    qint32 getResult() const;
    void setResult(qint32 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<CreateMoodstripResultData> d;
};

class CreateMoodstripResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, qint32 result)
    {
       CreateMoodstripResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        CreateMoodstripResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, 0);
    }

signals:
    void callbackSignal(QVariant state, Fault fault, qint32 result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, qint32 result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, qint32)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("CreateMoodstripResult", SIGNAL(callbackSignal(QVariant, Fault, qint32)), callback.method);
        }
    }
};

}

#endif // CREATEMOODSTRIPRESULT_H