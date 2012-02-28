#include "listwrapperobjects.h"
#include "getcontact.h"

namespace MoodBox
{

GetContactData::GetContactData() : QSharedData()
{
    this->userId = 0;
}
GetContactData::GetContactData(qint32 userId) : QSharedData()
{
    this->userId = userId;
}

GetContactData::~GetContactData()
{
}

GetContact::GetContact() : TransportableObject()
{
}
GetContact::GetContact(qint32 userId) : TransportableObject()
{
    d = new GetContactData(userId);
}

GetContact::~GetContact()
{
}

qint32 GetContact::getUserId() const
{
    Q_ASSERT_X(!isNull(), "GetContact::getUserId", "Getter call on object which isNull");
    return this->d->userId;
}
void GetContact::setUserId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "GetContact::setUserId", "Setter call on object which isNull");
    this->d->userId = value;
}

qint32 GetContact::getRepresentedTypeId()
{
    return 10081;
}

qint32 GetContact::getTypeId() const
{
    return 10081;
}
void GetContact::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->userId);
}
PropertyReadResult GetContact::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->userId = reader->readInt32();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
