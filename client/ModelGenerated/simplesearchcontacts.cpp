#include "listwrapperobjects.h"
#include "simplesearchcontacts.h"

namespace MoodBox
{

SimpleSearchContactsData::SimpleSearchContactsData() : QSharedData()
{
    this->pageNumber = 0;
    this->recordsPerPage = 0;
}
SimpleSearchContactsData::SimpleSearchContactsData(qint32 pageNumber, qint32 recordsPerPage, QString value) : QSharedData()
{
    this->pageNumber = pageNumber;
    this->recordsPerPage = recordsPerPage;
    this->value = value;
}

SimpleSearchContactsData::~SimpleSearchContactsData()
{
}

SimpleSearchContacts::SimpleSearchContacts() : TransportableObject()
{
}
SimpleSearchContacts::SimpleSearchContacts(qint32 pageNumber, qint32 recordsPerPage, QString value) : TransportableObject()
{
    d = new SimpleSearchContactsData(pageNumber, recordsPerPage, value);
}

SimpleSearchContacts::~SimpleSearchContacts()
{
}

qint32 SimpleSearchContacts::getPageNumber() const
{
    Q_ASSERT_X(!isNull(), "SimpleSearchContacts::getPageNumber", "Getter call on object which isNull");
    return this->d->pageNumber;
}
void SimpleSearchContacts::setPageNumber(qint32 value)
{
    Q_ASSERT_X(!isNull(), "SimpleSearchContacts::setPageNumber", "Setter call on object which isNull");
    this->d->pageNumber = value;
}
qint32 SimpleSearchContacts::getRecordsPerPage() const
{
    Q_ASSERT_X(!isNull(), "SimpleSearchContacts::getRecordsPerPage", "Getter call on object which isNull");
    return this->d->recordsPerPage;
}
void SimpleSearchContacts::setRecordsPerPage(qint32 value)
{
    Q_ASSERT_X(!isNull(), "SimpleSearchContacts::setRecordsPerPage", "Setter call on object which isNull");
    this->d->recordsPerPage = value;
}
QString SimpleSearchContacts::getValue() const
{
    Q_ASSERT_X(!isNull(), "SimpleSearchContacts::getValue", "Getter call on object which isNull");
    return this->d->value;
}
void SimpleSearchContacts::setValue(QString value)
{
    Q_ASSERT_X(!isNull(), "SimpleSearchContacts::setValue", "Setter call on object which isNull");
    this->d->value = value;
}

qint32 SimpleSearchContacts::getRepresentedTypeId()
{
    return 10033;
}

qint32 SimpleSearchContacts::getTypeId() const
{
    return 10033;
}
void SimpleSearchContacts::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->pageNumber);
    writer->writeProperty(this, 2, this->d->recordsPerPage);
    writer->writeProperty(this, 3, this->d->value);
}
PropertyReadResult SimpleSearchContacts::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
    }

    return PropertyReadResult(false);
}

}
