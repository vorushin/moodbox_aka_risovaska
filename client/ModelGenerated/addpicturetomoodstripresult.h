#ifndef ADDPICTURETOMOODSTRIPRESULT_H
#define ADDPICTURETOMOODSTRIPRESULT_H

#include <QObject>
#include <QSharedData>

#include "publishingmoodstripresult.h"
#include "transportableobject.h"
#include "fault.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class AddPictureToMoodstripResultData : public QSharedData
{
public:
    AddPictureToMoodstripResultData();
    AddPictureToMoodstripResultData(PublishingMoodstripResult result);
    virtual ~AddPictureToMoodstripResultData();

    PublishingMoodstripResult result;
};

class AddPictureToMoodstripResult : public TransportableObject
{
public:
    AddPictureToMoodstripResult();
    AddPictureToMoodstripResult(PublishingMoodstripResult result);
    virtual ~AddPictureToMoodstripResult();

protected:
    AddPictureToMoodstripResult(AddPictureToMoodstripResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static AddPictureToMoodstripResult* ___new_()
    {
        return new AddPictureToMoodstripResult(new AddPictureToMoodstripResultData());
    }
    static AddPictureToMoodstripResult empty()
    {
        return AddPictureToMoodstripResult(new AddPictureToMoodstripResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    PublishingMoodstripResult getResult() const;
    void setResult(PublishingMoodstripResult value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<AddPictureToMoodstripResultData> d;
};

class AddPictureToMoodstripResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, PublishingMoodstripResult result)
    {
       AddPictureToMoodstripResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        AddPictureToMoodstripResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, PublishingMoodstripResult());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, PublishingMoodstripResult result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, PublishingMoodstripResult result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, PublishingMoodstripResult)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("AddPictureToMoodstripResult", SIGNAL(callbackSignal(QVariant, Fault, PublishingMoodstripResult)), callback.method);
        }
    }
};

}

#endif // ADDPICTURETOMOODSTRIPRESULT_H