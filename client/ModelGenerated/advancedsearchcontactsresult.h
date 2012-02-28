#ifndef ADVANCEDSEARCHCONTACTSRESULT_H
#define ADVANCEDSEARCHCONTACTSRESULT_H

#include <QObject>
#include <QSharedData>

#include "usersearchresult.h"
#include "transportableobject.h"
#include "fault.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class AdvancedSearchContactsResultData : public QSharedData
{
public:
    AdvancedSearchContactsResultData();
    AdvancedSearchContactsResultData(UserSearchResult result);
    virtual ~AdvancedSearchContactsResultData();

    UserSearchResult result;
};

class AdvancedSearchContactsResult : public TransportableObject
{
public:
    AdvancedSearchContactsResult();
    AdvancedSearchContactsResult(UserSearchResult result);
    virtual ~AdvancedSearchContactsResult();

protected:
    AdvancedSearchContactsResult(AdvancedSearchContactsResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static AdvancedSearchContactsResult* ___new_()
    {
        return new AdvancedSearchContactsResult(new AdvancedSearchContactsResultData());
    }
    static AdvancedSearchContactsResult empty()
    {
        return AdvancedSearchContactsResult(new AdvancedSearchContactsResultData());
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
    QExplicitlySharedDataPointer<AdvancedSearchContactsResultData> d;
};

class AdvancedSearchContactsResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, UserSearchResult result)
    {
       AdvancedSearchContactsResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        AdvancedSearchContactsResultCallbackCaller caller;
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
            CallbackCallerTools::onConnectFail("AdvancedSearchContactsResult", SIGNAL(callbackSignal(QVariant, Fault, UserSearchResult)), callback.method);
        }
    }
};

}

#endif // ADVANCEDSEARCHCONTACTSRESULT_H