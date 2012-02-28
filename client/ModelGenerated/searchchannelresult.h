#ifndef SEARCHCHANNELRESULT_H
#define SEARCHCHANNELRESULT_H

#include <QObject>
#include <QSharedData>

#include "channelsearchresult.h"
#include "transportableobject.h"
#include "fault.h"
#include "callbackcallertools.h"

namespace MoodBox
{

class SearchChannelResultData : public QSharedData
{
public:
    SearchChannelResultData();
    SearchChannelResultData(ChannelSearchResult result);
    virtual ~SearchChannelResultData();

    ChannelSearchResult result;
};

class SearchChannelResult : public TransportableObject
{
public:
    SearchChannelResult();
    SearchChannelResult(ChannelSearchResult result);
    virtual ~SearchChannelResult();

protected:
    SearchChannelResult(SearchChannelResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static SearchChannelResult* ___new_()
    {
        return new SearchChannelResult(new SearchChannelResultData());
    }
    static SearchChannelResult empty()
    {
        return SearchChannelResult(new SearchChannelResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    virtual void resultCall(Callback callback, QVariant state);

    ChannelSearchResult getResult() const;
    void setResult(ChannelSearchResult value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<SearchChannelResultData> d;
};

class SearchChannelResultCallbackCaller : public QObject
{
    Q_OBJECT

public:
    static void call(Callback &callback, QVariant state, ChannelSearchResult result)
    {
       SearchChannelResultCallbackCaller caller;
       caller.callInternal(callback, state, Fault(), result);
    }
    static void call(Callback &callback, QVariant state, Fault fault)
    {
        SearchChannelResultCallbackCaller caller;
        caller.callInternal(callback, state, fault, ChannelSearchResult());
    }

signals:
    void callbackSignal(QVariant state, Fault fault, ChannelSearchResult result);

private:
    void callInternal(Callback &callback, QVariant state, Fault fault, ChannelSearchResult result)
    {
        if(connect(this, SIGNAL(callbackSignal(QVariant, Fault, ChannelSearchResult)), callback.target, callback.method))
        {
            emit callbackSignal(state, fault, result);
            disconnect();
        }
        else
	   {
            CallbackCallerTools::onConnectFail("SearchChannelResult", SIGNAL(callbackSignal(QVariant, Fault, ChannelSearchResult)), callback.method);
        }
    }
};

}

#endif // SEARCHCHANNELRESULT_H