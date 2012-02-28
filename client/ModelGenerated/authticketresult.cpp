#include "listwrapperobjects.h"
#include "authticketresult.h"

namespace MoodBox
{

AuthTicketResultData::AuthTicketResultData() : QSharedData()
{
    this->userId = 0;
    this->lifetime = 0;
    this->resultCode = AuthTicketResultCode::Ok;
    this->lockTime = 0;
}
AuthTicketResultData::AuthTicketResultData(QByteArray authTicket, qint32 userId, qint32 lifetime, AuthTicketResultCode::AuthTicketResultCodeEnum resultCode, qint32 lockTime) : QSharedData()
{
    this->authTicket = authTicket;
    this->userId = userId;
    this->lifetime = lifetime;
    this->resultCode = resultCode;
    this->lockTime = lockTime;
}

AuthTicketResultData::~AuthTicketResultData()
{
}

AuthTicketResult::AuthTicketResult() : TransportableObject()
{
}
AuthTicketResult::AuthTicketResult(QByteArray authTicket, qint32 userId, qint32 lifetime, AuthTicketResultCode::AuthTicketResultCodeEnum resultCode, qint32 lockTime) : TransportableObject()
{
    d = new AuthTicketResultData(authTicket, userId, lifetime, resultCode, lockTime);
}

AuthTicketResult::~AuthTicketResult()
{
}

QByteArray AuthTicketResult::getAuthTicket() const
{
    Q_ASSERT_X(!isNull(), "AuthTicketResult::getAuthTicket", "Getter call on object which isNull");
    return this->d->authTicket;
}
void AuthTicketResult::setAuthTicket(QByteArray value)
{
    Q_ASSERT_X(!isNull(), "AuthTicketResult::setAuthTicket", "Setter call on object which isNull");
    this->d->authTicket = value;
}
qint32 AuthTicketResult::getUserId() const
{
    Q_ASSERT_X(!isNull(), "AuthTicketResult::getUserId", "Getter call on object which isNull");
    return this->d->userId;
}
void AuthTicketResult::setUserId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "AuthTicketResult::setUserId", "Setter call on object which isNull");
    this->d->userId = value;
}
qint32 AuthTicketResult::getLifetime() const
{
    Q_ASSERT_X(!isNull(), "AuthTicketResult::getLifetime", "Getter call on object which isNull");
    return this->d->lifetime;
}
void AuthTicketResult::setLifetime(qint32 value)
{
    Q_ASSERT_X(!isNull(), "AuthTicketResult::setLifetime", "Setter call on object which isNull");
    this->d->lifetime = value;
}
AuthTicketResultCode::AuthTicketResultCodeEnum AuthTicketResult::getResultCode() const
{
    Q_ASSERT_X(!isNull(), "AuthTicketResult::getResultCode", "Getter call on object which isNull");
    return this->d->resultCode;
}
void AuthTicketResult::setResultCode(AuthTicketResultCode::AuthTicketResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "AuthTicketResult::setResultCode", "Setter call on object which isNull");
    this->d->resultCode = value;
}
qint32 AuthTicketResult::getLockTime() const
{
    Q_ASSERT_X(!isNull(), "AuthTicketResult::getLockTime", "Getter call on object which isNull");
    return this->d->lockTime;
}
void AuthTicketResult::setLockTime(qint32 value)
{
    Q_ASSERT_X(!isNull(), "AuthTicketResult::setLockTime", "Setter call on object which isNull");
    this->d->lockTime = value;
}

qint32 AuthTicketResult::getRepresentedTypeId()
{
    return 16;
}

qint32 AuthTicketResult::getTypeId() const
{
    return 16;
}
void AuthTicketResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->authTicket);
    writer->writeProperty(this, 2, this->d->userId);
    writer->writeProperty(this, 3, this->d->lifetime);
    writer->writeEnumProperty(this, 4, 20017, this->d->resultCode);
    writer->writeProperty(this, 5, this->d->lockTime);
}
PropertyReadResult AuthTicketResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->authTicket = reader->readBytes();
            return PropertyReadResult(true);
        case 2:
            this->d->userId = reader->readInt32();
            return PropertyReadResult(true);
        case 3:
            this->d->lifetime = reader->readInt32();
            return PropertyReadResult(true);
        case 4:
            this->d->resultCode = (AuthTicketResultCode::AuthTicketResultCodeEnum)reader->readEnum(20017);
            return PropertyReadResult(true);
        case 5:
            this->d->lockTime = reader->readInt32();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
