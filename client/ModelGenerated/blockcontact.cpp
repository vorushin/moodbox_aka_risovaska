#include "listwrapperobjects.h"
#include "blockcontact.h"

namespace MoodBox
{

BlockContactData::BlockContactData() : QSharedData()
{
    this->contactUserId = 0;
}
BlockContactData::BlockContactData(qint32 contactUserId) : QSharedData()
{
    this->contactUserId = contactUserId;
}

BlockContactData::~BlockContactData()
{
}

BlockContact::BlockContact() : TransportableObject()
{
}
BlockContact::BlockContact(qint32 contactUserId) : TransportableObject()
{
    d = new BlockContactData(contactUserId);
}

BlockContact::~BlockContact()
{
}

qint32 BlockContact::getContactUserId() const
{
    Q_ASSERT_X(!isNull(), "BlockContact::getContactUserId", "Getter call on object which isNull");
    return this->d->contactUserId;
}
void BlockContact::setContactUserId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "BlockContact::setContactUserId", "Setter call on object which isNull");
    this->d->contactUserId = value;
}

qint32 BlockContact::getRepresentedTypeId()
{
    return 10029;
}

qint32 BlockContact::getTypeId() const
{
    return 10029;
}
void BlockContact::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->contactUserId);
}
PropertyReadResult BlockContact::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
