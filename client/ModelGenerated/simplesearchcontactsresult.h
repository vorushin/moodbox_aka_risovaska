#ifndef SIMPLESEARCHCONTACTSRESULT_H
#define SIMPLESEARCHCONTACTSRESULT_H

#include <QObject>
#include <QSharedData>

#include "usersearchresult.h"
#include "transportableobject.h"
#include "fault.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class SimpleSearchContactsResultData : public QSharedData
{
public:
    SimpleSearchContactsResultData();
    SimpleSearchContactsResultData(UserSearchResult result);
    virtual ~SimpleSearchContactsResultData();

    UserSearchResult result;
};

class SimpleSearchContactsResult : public TransportableObject
{
public:
    SimpleSearchContactsResult();
    SimpleSearchContactsResult(UserSearchResult result);
    virtual ~SimpleSearchContactsResult();

protected:
    SimpleSearchContactsResult(SimpleSearchContactsResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static SimpleSearchContactsResult* ___new_()
    {
        return new SimpleSearchContactsResult(new SimpleSearchContactsResultData());
    }
    static SimpleSearchContactsResult empty()
    {
        return SimpleSearchContactsResult(new SimpleSearchContactsResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    UserSearchResult getResult() const;
    void setResult(UserSearchResult value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<SimpleSearchContactsResultData> d;
};

class SimpleSearchContactsResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, UserSearchResult result)
    {
       SimpleSearchContactsResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        SimpleSearchContactsResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, UserSearchResult());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, UserSearchResult result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, UserSearchResult result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, UserSearchResult)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("SimpleSearchContactsResult", SIGNAL(callbackSignal(QVariant, Fault, UserSearchResult)), callback.method);
        }
    }
};

}

#endif // SIMPLESEARCHCONTACTSRESULT_H