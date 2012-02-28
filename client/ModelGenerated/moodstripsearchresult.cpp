#include "listwrapperobjects.h"
#include "moodstripsearchresult.h"

namespace MoodBox
{

MoodstripSearchResultData::MoodstripSearchResultData() : QSharedData()
{
    this->pageNumber = 0;
    this->hasMore = false;
}
MoodstripSearchResultData::MoodstripSearchResultData(qint32 pageNumber, bool hasMore, QList<MoodstripResult> items) : QSharedData()
{
    this->pageNumber = pageNumber;
    this->hasMore = hasMore;
    this->items = items;
}

MoodstripSearchResultData::~MoodstripSearchResultData()
{
}

MoodstripSearchResult::MoodstripSearchResult() : TransportableObject()
{
}
MoodstripSearchResult::MoodstripSearchResult(qint32 pageNumber, bool hasMore, QList<MoodstripResult> items) : TransportableObject()
{
    d = new MoodstripSearchResultData(pageNumber, hasMore, items);
}

MoodstripSearchResult::~MoodstripSearchResult()
{
}

qint32 MoodstripSearchResult::getPageNumber() const
{
    Q_ASSERT_X(!isNull(), "MoodstripSearchResult::getPageNumber", "Getter call on object which isNull");
    return this->d->pageNumber;
}
void MoodstripSearchResult::setPageNumber(qint32 value)
{
    Q_ASSERT_X(!isNull(), "MoodstripSearchResult::setPageNumber", "Setter call on object which isNull");
    this->d->pageNumber = value;
}
bool MoodstripSearchResult::getHasMore() const
{
    Q_ASSERT_X(!isNull(), "MoodstripSearchResult::getHasMore", "Getter call on object which isNull");
    return this->d->hasMore;
}
void MoodstripSearchResult::setHasMore(bool value)
{
    Q_ASSERT_X(!isNull(), "MoodstripSearchResult::setHasMore", "Setter call on object which isNull");
    this->d->hasMore = value;
}
QList<MoodstripResult> MoodstripSearchResult::getItems() const
{
    Q_ASSERT_X(!isNull(), "MoodstripSearchResult::getItems", "Getter call on object which isNull");
    return this->d->items;
}
void MoodstripSearchResult::setItems(QList<MoodstripResult> value)
{
    Q_ASSERT_X(!isNull(), "MoodstripSearchResult::setItems", "Setter call on object which isNull");
    this->d->items = value;
}

qint32 MoodstripSearchResult::getRepresentedTypeId()
{
    return 15;
}

qint32 MoodstripSearchResult::getTypeId() const
{
    return 15;
}
void MoodstripSearchResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->pageNumber);
    writer->writeProperty(this, 2, this->d->hasMore);
    TransportableListOfSharedWrapper<MoodstripResult> items_wrapper(this->d->items);
    writer->writeProperty(this, 3, &items_wrapper);
}
PropertyReadResult MoodstripSearchResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
            this->d->items = QList<MoodstripResult>();
            return PropertyReadResult(new ListOfSharedWrapperObject<MoodstripResult>(&this->d->items, PropertyInfo(true, false)));
    }

    return PropertyReadResult(false);
}

}
