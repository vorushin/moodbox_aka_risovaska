#include "listwrapperobjects.h"
#include "unblockcontact.h"

namespace MoodBox
{

UnblockContactData::UnblockContactData() : QSharedData()
{
    this->contactUserId = 0;
}
UnblockContactData::UnblockContactData(qint32 contactUserId) : QSharedData()
{
    this->contactUserId = contactUserId;
}

UnblockContactData::~UnblockContactData()
{
}

UnblockContact::UnblockContact() : TransportableObject()
{
}
UnblockContact::UnblockContact(qint32 contactUserId) : TransportableObject()
{
    d = new UnblockContactData(contactUserId);
}

UnblockContact::~UnblockContact()
{
}

qint32 UnblockContact::getContactUserId() const
{
    Q_ASSERT_X(!isNull(), "UnblockContact::getContactUserId", "Getter call on object which isNull");
    return this->d->contactUserId;
}
void UnblockContact::setContactUserId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "UnblockContact::setContactUserId", "Setter call on object which isNull");
    this->d->contactUserId = value;
}

qint32 UnblockContact::getRepresentedTypeId()
{
    return 10031;
}

qint32 UnblockContact::getTypeId() const
{
    return 10031;
}
void UnblockContact::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->contactUserId);
}
PropertyReadResult UnblockContact::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
