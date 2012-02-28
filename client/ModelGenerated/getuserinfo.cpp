#include "listwrapperobjects.h"
#include "getuserinfo.h"

namespace MoodBox
{

GetUserInfoData::GetUserInfoData() : QSharedData()
{
    this->userId = 0;
}
GetUserInfoData::GetUserInfoData(qint32 userId) : QSharedData()
{
    this->userId = userId;
}

GetUserInfoData::~GetUserInfoData()
{
}

GetUserInfo::GetUserInfo() : TransportableObject()
{
}
GetUserInfo::GetUserInfo(qint32 userId) : TransportableObject()
{
    d = new GetUserInfoData(userId);
}

GetUserInfo::~GetUserInfo()
{
}

qint32 GetUserInfo::getUserId() const
{
    Q_ASSERT_X(!isNull(), "GetUserInfo::getUserId", "Getter call on object which isNull");
    return this->d->userId;
}
void GetUserInfo::setUserId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "GetUserInfo::setUserId", "Setter call on object which isNull");
    this->d->userId = value;
}

qint32 GetUserInfo::getRepresentedTypeId()
{
    return 10039;
}

qint32 GetUserInfo::getTypeId() const
{
    return 10039;
}
void GetUserInfo::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->userId);
}
PropertyReadResult GetUserInfo::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
