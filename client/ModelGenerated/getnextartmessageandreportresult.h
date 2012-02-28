#ifndef GETNEXTARTMESSAGEANDREPORTRESULT_H
#define GETNEXTARTMESSAGEANDREPORTRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "artmessage.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class GetNextArtmessageAndReportResultData : public QSharedData
{
public:
    GetNextArtmessageAndReportResultData();
    GetNextArtmessageAndReportResultData(ArtMessage result);
    virtual ~GetNextArtmessageAndReportResultData();

    ArtMessage result;
};

class GetNextArtmessageAndReportResult : public TransportableObject
{
public:
    GetNextArtmessageAndReportResult();
    GetNextArtmessageAndReportResult(ArtMessage result);
    virtual ~GetNextArtmessageAndReportResult();

protected:
    GetNextArtmessageAndReportResult(GetNextArtmessageAndReportResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetNextArtmessageAndReportResult* ___new_()
    {
        return new GetNextArtmessageAndReportResult(new GetNextArtmessageAndReportResultData());
    }
    static GetNextArtmessageAndReportResult empty()
    {
        return GetNextArtmessageAndReportResult(new GetNextArtmessageAndReportResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    ArtMessage getResult() const;
    void setResult(ArtMessage value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetNextArtmessageAndReportResultData> d;
};

class GetNextArtmessageAndReportResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, ArtMessage result)
    {
       GetNextArtmessageAndReportResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        GetNextArtmessageAndReportResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, ArtMessage());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, ArtMessage result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, ArtMessage result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, ArtMessage)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("GetNextArtmessageAndReportResult", SIGNAL(callbackSignal(QVariant, Fault, ArtMessage)), callback.method);
        }
    }
};

}

#endif // GETNEXTARTMESSAGEANDREPORTRESULT_H