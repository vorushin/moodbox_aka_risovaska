#ifndef CHANNELSEARCHRESULT_H
#define CHANNELSEARCHRESULT_H

#include <QSharedData>
#include <QList>

#include "channelresult.h"
#include "transportableobject.h"

namespace MoodBox
{

class ChannelSearchResultData : public QSharedData
{
public:
    ChannelSearchResultData();
    ChannelSearchResultData(qint32 pageNumber, bool hasMore, QList<ChannelResult> items);
    virtual ~ChannelSearchResultData();

    qint32 pageNumber;
    bool hasMore;
    QList<ChannelResult> items;
};

class ChannelSearchResult : public TransportableObject
{
public:
    ChannelSearchResult();
    ChannelSearchResult(qint32 pageNumber, bool hasMore, QList<ChannelResult> items);
    virtual ~ChannelSearchResult();

protected:
    ChannelSearchResult(ChannelSearchResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static ChannelSearchResult* ___new_()
    {
        return new ChannelSearchResult(new ChannelSearchResultData());
    }
    static ChannelSearchResult empty()
    {
        return ChannelSearchResult(new ChannelSearchResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getPageNumber() const;
    void setPageNumber(qint32 value);
    bool getHasMore() const;
    void setHasMore(bool value);
    QList<ChannelResult> getItems() const;
    void setItems(QList<ChannelResult> value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<ChannelSearchResultData> d;
};

}

#endif // CHANNELSEARCHRESULT_H