#include "listwrapperobjects.h"
#include "removefromcontacts.h"

namespace MoodBox
{

RemoveFromContactsData::RemoveFromContactsData() : QSharedData()
{
    this->contactUserId = 0;
}
RemoveFromContactsData::RemoveFromContactsData(qint32 contactUserId) : QSharedData()
{
    this->contactUserId = contactUserId;
}

RemoveFromContactsData::~RemoveFromContactsData()
{
}

RemoveFromContacts::RemoveFromContacts() : TransportableObject()
{
}
RemoveFromContacts::RemoveFromContacts(qint32 contactUserId) : TransportableObject()
{
    d = new RemoveFromContactsData(contactUserId);
}

RemoveFromContacts::~RemoveFromContacts()
{
}

qint32 RemoveFromContacts::getContactUserId() const
{
    Q_ASSERT_X(!isNull(), "RemoveFromContacts::getContactUserId", "Getter call on object which isNull");
    return this->d->contactUserId;
}
void RemoveFromContacts::setContactUserId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "RemoveFromContacts::setContactUserId", "Setter call on object which isNull");
    this->d->contactUserId = value;
}

qint32 RemoveFromContacts::getRepresentedTypeId()
{
    return 10019;
}

qint32 RemoveFromContacts::getTypeId() const
{
    return 10019;
}
void RemoveFromContacts::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->contactUserId);
}
PropertyReadResult RemoveFromContacts::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->contactUserId = reader->readInt32();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
