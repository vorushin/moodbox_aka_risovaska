#include "listwrapperobjects.h"
#include "getauthorization.h"

namespace MoodBox
{

GetAuthorizationData::GetAuthorizationData() : QSharedData()
{
    this->userId = 0;
}
GetAuthorizationData::GetAuthorizationData(qint32 userId) : QSharedData()
{
    this->userId = userId;
}

GetAuthorizationData::~GetAuthorizationData()
{
}

GetAuthorization::GetAuthorization() : TransportableObject()
{
}
GetAuthorization::GetAuthorization(qint32 userId) : TransportableObject()
{
    d = new GetAuthorizationData(userId);
}

GetAuthorization::~GetAuthorization()
{
}

qint32 GetAuthorization::getUserId() const
{
    Q_ASSERT_X(!isNull(), "GetAuthorization::getUserId", "Getter call on object which isNull");
    return this->d->userId;
}
void GetAuthorization::setUserId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "GetAuthorization::setUserId", "Setter call on object which isNull");
    this->d->userId = value;
}

qint32 GetAuthorization::getRepresentedTypeId()
{
    return 10085;
}

qint32 GetAuthorization::getTypeId() const
{
    return 10085;
}
void GetAuthorization::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->userId);
}
PropertyReadResult GetAuthorization::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
