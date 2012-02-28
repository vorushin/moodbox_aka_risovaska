#ifndef USERSEARCHRESULT_H
#define USERSEARCHRESULT_H

#include <QSharedData>
#include <QList>

#include "transportableobject.h"
#include "userinfo.h"

namespace MoodBox
{

class UserSearchResultData : public QSharedData
{
public:
    UserSearchResultData();
    UserSearchResultData(qint32 pageNumber, bool hasMore, QList<UserInfo> items);
    virtual ~UserSearchResultData();

    qint32 pageNumber;
    bool hasMore;
    QList<UserInfo> items;
};

class UserSearchResult : public TransportableObject
{
public:
    UserSearchResult();
    UserSearchResult(qint32 pageNumber, bool hasMore, QList<UserInfo> items);
    virtual ~UserSearchResult();

protected:
    UserSearchResult(UserSearchResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static UserSearchResult* ___new_()
    {
        return new UserSearchResult(new UserSearchResultData());
    }
    static UserSearchResult empty()
    {
        return UserSearchResult(new UserSearchResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getPageNumber() const;
    void setPageNumber(qint32 value);
    bool getHasMore() const;
    void setHasMore(bool value);
    QList<UserInfo> getItems() const;
    void setItems(QList<UserInfo> value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<UserSearchResultData> d;
};

}

#endif // USERSEARCHRESULT_H