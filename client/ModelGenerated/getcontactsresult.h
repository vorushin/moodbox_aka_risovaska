#ifndef GETCONTACTSRESULT_H
#define GETCONTACTSRESULT_H

#include <QObject>
#include <QSharedData>
#include <QList>

#include "transportableobject.h"
#include "fault.h"
#include "contactinfo.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class GetContactsResultData : public QSharedData
{
public:
    GetContactsResultData();
    GetContactsResultData(QList<ContactInfo> result);
    virtual ~GetContactsResultData();

    QList<ContactInfo> result;
};

class GetContactsResult : public TransportableObject
{
public:
    GetContactsResult();
    GetContactsResult(QList<ContactInfo> result);
    virtual ~GetContactsResult();

protected:
    GetContactsResult(GetContactsResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetContactsResult* ___new_()
    {
        return new GetContactsResult(new GetContactsResultData());
    }
    static GetContactsResult empty()
    {
        return GetContactsResult(new GetContactsResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    QList<ContactInfo> getResult() const;
    void setResult(QList<ContactInfo> value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetContactsResultData> d;
};

class GetContactsResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, QList<ContactInfo> result)
    {
       GetContactsResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        GetContactsResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, QList<ContactInfo>());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, QList<ContactInfo> result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, QList<ContactInfo> result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, QList<ContactInfo>)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("GetContactsResult", SIGNAL(callbackSignal(QVariant, Fault, QList<ContactInfo>)), callback.method);
        }
    }
};

}

#endif // GETCONTACTSRESULT_H