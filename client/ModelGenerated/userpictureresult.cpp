#include "listwrapperobjects.h"
#include "userpictureresult.h"

namespace MoodBox
{

UserPictureResultData::UserPictureResultData() : QSharedData()
{
    this->resultCode = UserPictureResultCode::Ok;
    this->userId = 0;
}
UserPictureResultData::UserPictureResultData(UserPictureResultCode::UserPictureResultCodeEnum resultCode, qint32 userId, QDateTime lastChangeDate, QByteArray pictureData) : QSharedData()
{
    this->resultCode = resultCode;
    this->userId = userId;
    this->lastChangeDate = lastChangeDate;
    this->pictureData = pictureData;
}

UserPictureResultData::~UserPictureResultData()
{
}

UserPictureResult::UserPictureResult() : TransportableObject()
{
}
UserPictureResult::UserPictureResult(UserPictureResultCode::UserPictureResultCodeEnum resultCode, qint32 userId, QDateTime lastChangeDate, QByteArray pictureData) : TransportableObject()
{
    d = new UserPictureResultData(resultCode, userId, lastChangeDate, pictureData);
}

UserPictureResult::~UserPictureResult()
{
}

UserPictureResultCode::UserPictureResultCodeEnum UserPictureResult::getResultCode() const
{
    Q_ASSERT_X(!isNull(), "UserPictureResult::getResultCode", "Getter call on object which isNull");
    return this->d->resultCode;
}
void UserPictureResult::setResultCode(UserPictureResultCode::UserPictureResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "UserPictureResult::setResultCode", "Setter call on object which isNull");
    this->d->resultCode = value;
}
qint32 UserPictureResult::getUserId() const
{
    Q_ASSERT_X(!isNull(), "UserPictureResult::getUserId", "Getter call on object which isNull");
    return this->d->userId;
}
void UserPictureResult::setUserId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "UserPictureResult::setUserId", "Setter call on object which isNull");
    this->d->userId = value;
}
QDateTime UserPictureResult::getLastChangeDate() const
{
    Q_ASSERT_X(!isNull(), "UserPictureResult::getLastChangeDate", "Getter call on object which isNull");
    return this->d->lastChangeDate;
}
void UserPictureResult::setLastChangeDate(QDateTime value)
{
    Q_ASSERT_X(!isNull(), "UserPictureResult::setLastChangeDate", "Setter call on object which isNull");
    this->d->lastChangeDate = value;
}
QByteArray UserPictureResult::getPictureData() const
{
    Q_ASSERT_X(!isNull(), "UserPictureResult::getPictureData", "Getter call on object which isNull");
    return this->d->pictureData;
}
void UserPictureResult::setPictureData(QByteArray value)
{
    Q_ASSERT_X(!isNull(), "UserPictureResult::setPictureData", "Setter call on object which isNull");
    this->d->pictureData = value;
}

qint32 UserPictureResult::getRepresentedTypeId()
{
    return 5;
}

qint32 UserPictureResult::getTypeId() const
{
    return 5;
}
void UserPictureResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20010, this->d->resultCode);
    writer->writeProperty(this, 2, this->d->userId);
    writer->writeProperty(this, 3, this->d->lastChangeDate);
    writer->writeProperty(this, 4, this->d->pictureData);
}
PropertyReadResult UserPictureResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->resultCode = (UserPictureResultCode::UserPictureResultCodeEnum)reader->readEnum(20010);
            return PropertyReadResult(true);
        case 2:
            this->d->userId = reader->readInt32();
            return PropertyReadResult(true);
        case 3:
            this->d->lastChangeDate = reader->readDateTime();
            return PropertyReadResult(true);
        case 4:
            this->d->pictureData = reader->readBytes();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
