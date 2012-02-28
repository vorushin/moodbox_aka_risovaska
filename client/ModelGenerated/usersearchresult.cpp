#include "listwrapperobjects.h"
#include "usersearchresult.h"

namespace MoodBox
{

UserSearchResultData::UserSearchResultData() : QSharedData()
{
    this->pageNumber = 0;
    this->hasMore = false;
}
UserSearchResultData::UserSearchResultData(qint32 pageNumber, bool hasMore, QList<UserInfo> items) : QSharedData()
{
    this->pageNumber = pageNumber;
    this->hasMore = hasMore;
    this->items = items;
}

UserSearchResultData::~UserSearchResultData()
{
}

UserSearchResult::UserSearchResult() : TransportableObject()
{
}
UserSearchResult::UserSearchResult(qint32 pageNumber, bool hasMore, QList<UserInfo> items) : TransportableObject()
{
    d = new UserSearchResultData(pageNumber, hasMore, items);
}

UserSearchResult::~UserSearchResult()
{
}

qint32 UserSearchResult::getPageNumber() const
{
    Q_ASSERT_X(!isNull(), "UserSearchResult::getPageNumber", "Getter call on object which isNull");
    return this->d->pageNumber;
}
void UserSearchResult::setPageNumber(qint32 value)
{
    Q_ASSERT_X(!isNull(), "UserSearchResult::setPageNumber", "Setter call on object which isNull");
    this->d->pageNumber = value;
}
bool UserSearchResult::getHasMore() const
{
    Q_ASSERT_X(!isNull(), "UserSearchResult::getHasMore", "Getter call on object which isNull");
    return this->d->hasMore;
}
void UserSearchResult::setHasMore(bool value)
{
    Q_ASSERT_X(!isNull(), "UserSearchResult::setHasMore", "Setter call on object which isNull");
    this->d->hasMore = value;
}
QList<UserInfo> UserSearchResult::getItems() const
{
    Q_ASSERT_X(!isNull(), "UserSearchResult::getItems", "Getter call on object which isNull");
    return this->d->items;
}
void UserSearchResult::setItems(QList<UserInfo> value)
{
    Q_ASSERT_X(!isNull(), "UserSearchResult::setItems", "Setter call on object which isNull");
    this->d->items = value;
}

qint32 UserSearchResult::getRepresentedTypeId()
{
    return 14;
}

qint32 UserSearchResult::getTypeId() const
{
    return 14;
}
void UserSearchResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->pageNumber);
    writer->writeProperty(this, 2, this->d->hasMore);
    TransportableListOfSharedWrapper<UserInfo> items_wrapper(this->d->items);
    writer->writeProperty(this, 3, &items_wrapper);
}
PropertyReadResult UserSearchResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
            this->d->items = QList<UserInfo>();
            return PropertyReadResult(new ListOfSharedWrapperObject<UserInfo>(&this->d->items, PropertyInfo(true, false)));
    }

    return PropertyReadResult(false);
}

}
