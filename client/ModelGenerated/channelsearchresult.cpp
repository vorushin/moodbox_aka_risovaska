#include "listwrapperobjects.h"
#include "channelsearchresult.h"

namespace MoodBox
{

ChannelSearchResultData::ChannelSearchResultData() : QSharedData()
{
    this->pageNumber = 0;
    this->hasMore = false;
}
ChannelSearchResultData::ChannelSearchResultData(qint32 pageNumber, bool hasMore, QList<ChannelResult> items) : QSharedData()
{
    this->pageNumber = pageNumber;
    this->hasMore = hasMore;
    this->items = items;
}

ChannelSearchResultData::~ChannelSearchResultData()
{
}

ChannelSearchResult::ChannelSearchResult() : TransportableObject()
{
}
ChannelSearchResult::ChannelSearchResult(qint32 pageNumber, bool hasMore, QList<ChannelResult> items) : TransportableObject()
{
    d = new ChannelSearchResultData(pageNumber, hasMore, items);
}

ChannelSearchResult::~ChannelSearchResult()
{
}

qint32 ChannelSearchResult::getPageNumber() const
{
    Q_ASSERT_X(!isNull(), "ChannelSearchResult::getPageNumber", "Getter call on object which isNull");
    return this->d->pageNumber;
}
void ChannelSearchResult::setPageNumber(qint32 value)
{
    Q_ASSERT_X(!isNull(), "ChannelSearchResult::setPageNumber", "Setter call on object which isNull");
    this->d->pageNumber = value;
}
bool ChannelSearchResult::getHasMore() const
{
    Q_ASSERT_X(!isNull(), "ChannelSearchResult::getHasMore", "Getter call on object which isNull");
    return this->d->hasMore;
}
void ChannelSearchResult::setHasMore(bool value)
{
    Q_ASSERT_X(!isNull(), "ChannelSearchResult::setHasMore", "Setter call on object which isNull");
    this->d->hasMore = value;
}
QList<ChannelResult> ChannelSearchResult::getItems() const
{
    Q_ASSERT_X(!isNull(), "ChannelSearchResult::getItems", "Getter call on object which isNull");
    return this->d->items;
}
void ChannelSearchResult::setItems(QList<ChannelResult> value)
{
    Q_ASSERT_X(!isNull(), "ChannelSearchResult::setItems", "Setter call on object which isNull");
    this->d->items = value;
}

qint32 ChannelSearchResult::getRepresentedTypeId()
{
    return 27;
}

qint32 ChannelSearchResult::getTypeId() const
{
    return 27;
}
void ChannelSearchResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->pageNumber);
    writer->writeProperty(this, 2, this->d->hasMore);
    TransportableListOfSharedWrapper<ChannelResult> items_wrapper(this->d->items);
    writer->writeProperty(this, 3, &items_wrapper);
}
PropertyReadResult ChannelSearchResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->pageNumber = reader->readInt32();
            return PropertyReadResult(true);
        case 2:
            this->d->hasMore = reader->readBool();
            return PropertyReadResult(true);
        case 3:
            this->d->items = QList<ChannelResult>();
            return PropertyReadResult(new ListOfSharedWrapperObject<ChannelResult>(&this->d->items, PropertyInfo(true, false)));
    }

    return PropertyReadResult(false);
}

}
