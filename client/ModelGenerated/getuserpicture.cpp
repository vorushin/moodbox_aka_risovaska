#include "listwrapperobjects.h"
#include "getuserpicture.h"

namespace MoodBox
{

GetUserPictureData::GetUserPictureData() : QSharedData()
{
    this->userId = 0;
}
GetUserPictureData::GetUserPictureData(qint32 userId, QDateTime lastChangedDate) : QSharedData()
{
    this->userId = userId;
    this->lastChangedDate = lastChangedDate;
}

GetUserPictureData::~GetUserPictureData()
{
}

GetUserPicture::GetUserPicture() : TransportableObject()
{
}
GetUserPicture::GetUserPicture(qint32 userId, QDateTime lastChangedDate) : TransportableObject()
{
    d = new GetUserPictureData(userId, lastChangedDate);
}

GetUserPicture::~GetUserPicture()
{
}

qint32 GetUserPicture::getUserId() const
{
    Q_ASSERT_X(!isNull(), "GetUserPicture::getUserId", "Getter call on object which isNull");
    return this->d->userId;
}
void GetUserPicture::setUserId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "GetUserPicture::setUserId", "Setter call on object which isNull");
    this->d->userId = value;
}
QDateTime GetUserPicture::getLastChangedDate() const
{
    Q_ASSERT_X(!isNull(), "GetUserPicture::getLastChangedDate", "Getter call on object which isNull");
    return this->d->lastChangedDate;
}
void GetUserPicture::setLastChangedDate(QDateTime value)
{
    Q_ASSERT_X(!isNull(), "GetUserPicture::setLastChangedDate", "Setter call on object which isNull");
    this->d->lastChangedDate = value;
}

qint32 GetUserPicture::getRepresentedTypeId()
{
    return 10009;
}

qint32 GetUserPicture::getTypeId() const
{
    return 10009;
}
void GetUserPicture::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->userId);
    writer->writeProperty(this, 2, this->d->lastChangedDate);
}
PropertyReadResult GetUserPicture::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->userId = reader->readInt32();
            return PropertyReadResult(true);
        case 2:
            this->d->lastChangedDate = reader->readDateTime();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
