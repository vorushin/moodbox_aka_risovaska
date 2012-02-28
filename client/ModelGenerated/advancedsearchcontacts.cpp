#include "listwrapperobjects.h"
#include "advancedsearchcontacts.h"

namespace MoodBox
{

AdvancedSearchContactsData::AdvancedSearchContactsData() : QSharedData()
{
    this->pageNumber = 0;
    this->recordsPerPage = 0;
    this->country = QLocale::AnyCountry;
    this->sex = Sex::Undefined;
    this->minAge = 0;
    this->maxAge = 0;
}
AdvancedSearchContactsData::AdvancedSearchContactsData(qint32 pageNumber, qint32 recordsPerPage, QString value, QLocale::Country country, QString city, Sex::SexEnum sex, qint32 minAge, qint32 maxAge) : QSharedData()
{
    this->pageNumber = pageNumber;
    this->recordsPerPage = recordsPerPage;
    this->value = value;
    this->country = country;
    this->city = city;
    this->sex = sex;
    this->minAge = minAge;
    this->maxAge = maxAge;
}

AdvancedSearchContactsData::~AdvancedSearchContactsData()
{
}

AdvancedSearchContacts::AdvancedSearchContacts() : TransportableObject()
{
}
AdvancedSearchContacts::AdvancedSearchContacts(qint32 pageNumber, qint32 recordsPerPage, QString value, QLocale::Country country, QString city, Sex::SexEnum sex, qint32 minAge, qint32 maxAge) : TransportableObject()
{
    d = new AdvancedSearchContactsData(pageNumber, recordsPerPage, value, country, city, sex, minAge, maxAge);
}

AdvancedSearchContacts::~AdvancedSearchContacts()
{
}

qint32 AdvancedSearchContacts::getPageNumber() const
{
    Q_ASSERT_X(!isNull(), "AdvancedSearchContacts::getPageNumber", "Getter call on object which isNull");
    return this->d->pageNumber;
}
void AdvancedSearchContacts::setPageNumber(qint32 value)
{
    Q_ASSERT_X(!isNull(), "AdvancedSearchContacts::setPageNumber", "Setter call on object which isNull");
    this->d->pageNumber = value;
}
qint32 AdvancedSearchContacts::getRecordsPerPage() const
{
    Q_ASSERT_X(!isNull(), "AdvancedSearchContacts::getRecordsPerPage", "Getter call on object which isNull");
    return this->d->recordsPerPage;
}
void AdvancedSearchContacts::setRecordsPerPage(qint32 value)
{
    Q_ASSERT_X(!isNull(), "AdvancedSearchContacts::setRecordsPerPage", "Setter call on object which isNull");
    this->d->recordsPerPage = value;
}
QString AdvancedSearchContacts::getValue() const
{
    Q_ASSERT_X(!isNull(), "AdvancedSearchContacts::getValue", "Getter call on object which isNull");
    return this->d->value;
}
void AdvancedSearchContacts::setValue(QString value)
{
    Q_ASSERT_X(!isNull(), "AdvancedSearchContacts::setValue", "Setter call on object which isNull");
    this->d->value = value;
}
QLocale::Country AdvancedSearchContacts::getCountry() const
{
    Q_ASSERT_X(!isNull(), "AdvancedSearchContacts::getCountry", "Getter call on object which isNull");
    return this->d->country;
}
void AdvancedSearchContacts::setCountry(QLocale::Country value)
{
    Q_ASSERT_X(!isNull(), "AdvancedSearchContacts::setCountry", "Setter call on object which isNull");
    this->d->country = value;
}
QString AdvancedSearchContacts::getCity() const
{
    Q_ASSERT_X(!isNull(), "AdvancedSearchContacts::getCity", "Getter call on object which isNull");
    return this->d->city;
}
void AdvancedSearchContacts::setCity(QString value)
{
    Q_ASSERT_X(!isNull(), "AdvancedSearchContacts::setCity", "Setter call on object which isNull");
    this->d->city = value;
}
Sex::SexEnum AdvancedSearchContacts::getSex() const
{
    Q_ASSERT_X(!isNull(), "AdvancedSearchContacts::getSex", "Getter call on object which isNull");
    return this->d->sex;
}
void AdvancedSearchContacts::setSex(Sex::SexEnum value)
{
    Q_ASSERT_X(!isNull(), "AdvancedSearchContacts::setSex", "Setter call on object which isNull");
    this->d->sex = value;
}
qint32 AdvancedSearchContacts::getMinAge() const
{
    Q_ASSERT_X(!isNull(), "AdvancedSearchContacts::getMinAge", "Getter call on object which isNull");
    return this->d->minAge;
}
void AdvancedSearchContacts::setMinAge(qint32 value)
{
    Q_ASSERT_X(!isNull(), "AdvancedSearchContacts::setMinAge", "Setter call on object which isNull");
    this->d->minAge = value;
}
qint32 AdvancedSearchContacts::getMaxAge() const
{
    Q_ASSERT_X(!isNull(), "AdvancedSearchContacts::getMaxAge", "Getter call on object which isNull");
    return this->d->maxAge;
}
void AdvancedSearchContacts::setMaxAge(qint32 value)
{
    Q_ASSERT_X(!isNull(), "AdvancedSearchContacts::setMaxAge", "Setter call on object which isNull");
    this->d->maxAge = value;
}

qint32 AdvancedSearchContacts::getRepresentedTypeId()
{
    return 10035;
}

qint32 AdvancedSearchContacts::getTypeId() const
{
    return 10035;
}
void AdvancedSearchContacts::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->pageNumber);
    writer->writeProperty(this, 2, this->d->recordsPerPage);
    writer->writeProperty(this, 3, this->d->value);
    writer->writeEnumProperty(this, 4, 20002, this->d->country);
    writer->writeProperty(this, 5, this->d->city);
    writer->writeEnumProperty(this, 6, 20001, this->d->sex);
    writer->writeProperty(this, 7, this->d->minAge);
    writer->writeProperty(this, 8, this->d->maxAge);
}
PropertyReadResult AdvancedSearchContacts::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
            this->d->recordsPerPage = reader->readInt32();
            return PropertyReadResult(true);
        case 3:
            this->d->value = reader->readString();
            return PropertyReadResult(true);
        case 4:
            this->d->country = (QLocale::Country)reader->readEnum(20002);
            return PropertyReadResult(true);
        case 5:
            this->d->city = reader->readString();
            return PropertyReadResult(true);
        case 6:
            this->d->sex = (Sex::SexEnum)reader->readEnum(20001);
            return PropertyReadResult(true);
        case 7:
            this->d->minAge = reader->readInt32();
            return PropertyReadResult(true);
        case 8:
            this->d->maxAge = reader->readInt32();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
