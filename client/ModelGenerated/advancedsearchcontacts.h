#ifndef ADVANCEDSEARCHCONTACTS_H
#define ADVANCEDSEARCHCONTACTS_H

#include <QSharedData>
#include <QLocale>
#include <QString>

#include "transportableobject.h"
#include "sex.h"
#include "country.h"

namespace MoodBox
{

class AdvancedSearchContactsData : public QSharedData
{
public:
    AdvancedSearchContactsData();
    AdvancedSearchContactsData(qint32 pageNumber, qint32 recordsPerPage, QString value, QLocale::Country country, QString city, Sex::SexEnum sex, qint32 minAge, qint32 maxAge);
    virtual ~AdvancedSearchContactsData();

    qint32 pageNumber;
    qint32 recordsPerPage;
    QString value;
    QLocale::Country country;
    QString city;
    Sex::SexEnum sex;
    qint32 minAge;
    qint32 maxAge;
};

class AdvancedSearchContacts : public TransportableObject
{
public:
    AdvancedSearchContacts();
    AdvancedSearchContacts(qint32 pageNumber, qint32 recordsPerPage, QString value, QLocale::Country country, QString city, Sex::SexEnum sex, qint32 minAge, qint32 maxAge);
    virtual ~AdvancedSearchContacts();

protected:
    AdvancedSearchContacts(AdvancedSearchContactsData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static AdvancedSearchContacts* ___new_()
    {
        return new AdvancedSearchContacts(new AdvancedSearchContactsData());
    }
    static AdvancedSearchContacts empty()
    {
        return AdvancedSearchContacts(new AdvancedSearchContactsData());
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
    QLocale::Country getCountry() const;
    void setCountry(QLocale::Country value);
    QString getCity() const;
    void setCity(QString value);
    Sex::SexEnum getSex() const;
    void setSex(Sex::SexEnum value);
    qint32 getMinAge() const;
    void setMinAge(qint32 value);
    qint32 getMaxAge() const;
    void setMaxAge(qint32 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<AdvancedSearchContactsData> d;
};

}

#endif // ADVANCEDSEARCHCONTACTS_H