#ifndef MOODSTRIPSEARCHRESULT_H
#define MOODSTRIPSEARCHRESULT_H

#include <QSharedData>
#include <QList>

#include "moodstripresult.h"
#include "transportableobject.h"

namespace MoodBox
{

class MoodstripSearchResultData : public QSharedData
{
public:
    MoodstripSearchResultData();
    MoodstripSearchResultData(qint32 pageNumber, bool hasMore, QList<MoodstripResult> items);
    virtual ~MoodstripSearchResultData();

    qint32 pageNumber;
    bool hasMore;
    QList<MoodstripResult> items;
};

class MoodstripSearchResult : public TransportableObject
{
public:
    MoodstripSearchResult();
    MoodstripSearchResult(qint32 pageNumber, bool hasMore, QList<MoodstripResult> items);
    virtual ~MoodstripSearchResult();

protected:
    MoodstripSearchResult(MoodstripSearchResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static MoodstripSearchResult* ___new_()
    {
        return new MoodstripSearchResult(new MoodstripSearchResultData());
    }
    static MoodstripSearchResult empty()
    {
        return MoodstripSearchResult(new MoodstripSearchResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getPageNumber() const;
    void setPageNumber(qint32 value);
    bool getHasMore() const;
    void setHasMore(bool value);
    QList<MoodstripResult> getItems() const;
    void setItems(QList<MoodstripResult> value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<MoodstripSearchResultData> d;
};

}

#endif // MOODSTRIPSEARCHRESULT_H