#include "listwrapperobjects.h"
#include "getstatus.h"

namespace MoodBox
{

GetStatusData::GetStatusData() : QSharedData()
{
    this->userId = 0;
}
GetStatusData::GetStatusData(qint32 userId) : QSharedData()
{
    this->userId = userId;
}

GetStatusData::~GetStatusData()
{
}

GetStatus::GetStatus() : TransportableObject()
{
}
GetStatus::GetStatus(qint32 userId) : TransportableObject()
{
    d = new GetStatusData(userId);
}

GetStatus::~GetStatus()
{
}

qint32 GetStatus::getUserId() const
{
    Q_ASSERT_X(!isNull(), "GetStatus::getUserId", "Getter call on object which isNull");
    return this->d->userId;
}
void GetStatus::setUserId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "GetStatus::setUserId", "Setter call on object which isNull");
    this->d->userId = value;
}

qint32 GetStatus::getRepresentedTypeId()
{
    return 10083;
}

qint32 GetStatus::getTypeId() const
{
    return 10083;
}
void GetStatus::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->userId);
}
PropertyReadResult GetStatus::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
