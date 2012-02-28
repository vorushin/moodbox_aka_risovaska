#ifndef GETCONTACTRESULT_H
#define GETCONTACTRESULT_H

#include <QObject>
#include <QSharedData>

#include "transportableobject.h"
#include "fault.h"
#include "contactinfo.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class GetContactResultData : public QSharedData
{
public:
    GetContactResultData();
    GetContactResultData(ContactInfo result);
    virtual ~GetContactResultData();

    ContactInfo result;
};

class GetContactResult : public TransportableObject
{
public:
    GetContactResult();
    GetContactResult(ContactInfo result);
    virtual ~GetContactResult();

protected:
    GetContactResult(GetContactResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetContactResult* ___new_()
    {
        return new GetContactResult(new GetContactResultData());
    }
    static GetContactResult empty()
    {
        return GetContactResult(new GetContactResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    ContactInfo getResult() const;
    void setResult(ContactInfo value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetContactResultData> d;
};

class GetContactResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, ContactInfo result)
    {
       GetContactResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        GetContactResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, ContactInfo());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, ContactInfo result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, ContactInfo result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, ContactInfo)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("GetContactResult", SIGNAL(callbackSignal(QVariant, Fault, ContactInfo)), callback.method);
        }
    }
};

}

#endif // GETCONTACTRESULT_H