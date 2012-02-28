#ifndef SIMPLESEARCHCONTACTS_H
#define SIMPLESEARCHCONTACTS_H

#include <QSharedData>
#include <QString>

#include "transportableobject.h"

namespace MoodBox
{

class SimpleSearchContactsData : public QSharedData
{
public:
    SimpleSearchContactsData();
    SimpleSearchContactsData(qint32 pageNumber, qint32 recordsPerPage, QString value);
    virtual ~SimpleSearchContactsData();

    qint32 pageNumber;
    qint32 recordsPerPage;
    QString value;
};

class SimpleSearchContacts : public TransportableObject
{
public:
    SimpleSearchContacts();
    SimpleSearchContacts(qint32 pageNumber, qint32 recordsPerPage, QString value);
    virtual ~SimpleSearchContacts();

protected:
    SimpleSearchContacts(SimpleSearchContactsData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static SimpleSearchContacts* ___new_()
    {
        return new SimpleSearchContacts(new SimpleSearchContactsData());
    }
    static SimpleSearchContacts empty()
    {
        return SimpleSearchContacts(new SimpleSearchContactsData());
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
    QExplicitlySharedDataPointer<SimpleSearchContactsData> d;
};

}

#endif // SIMPLESEARCHCONTACTS_H