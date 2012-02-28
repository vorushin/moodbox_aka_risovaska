#ifndef SEARCHCHANNEL_H
#define SEARCHCHANNEL_H

#include <QSharedData>
#include <QString>

#include "transportableobject.h"

namespace MoodBox
{

class SearchChannelData : public QSharedData
{
public:
    SearchChannelData();
    SearchChannelData(qint32 pageNumber, qint32 recordsPerPage, QString value);
    virtual ~SearchChannelData();

    qint32 pageNumber;
    qint32 recordsPerPage;
    QString value;
};

class SearchChannel : public TransportableObject
{
public:
    SearchChannel();
    SearchChannel(qint32 pageNumber, qint32 recordsPerPage, QString value);
    virtual ~SearchChannel();

protected:
    SearchChannel(SearchChannelData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static SearchChannel* ___new_()
    {
        return new SearchChannel(new SearchChannelData());
    }
    static SearchChannel empty()
    {
        return SearchChannel(new SearchChannelData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getPageNumber() const;
    void setPageNumber(qint32 value);
    qint32 getRecordsPerPage() const;
    void setRecordsPerPage(qint32 value);
    QString getValue() const;
    void setValue(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<SearchChannelData> d;
};

}

#endif // SEARCHCHANNEL_H