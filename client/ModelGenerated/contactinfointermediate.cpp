#include "listwrapperobjects.h"
#include "contactinfointermediate.h"

namespace MoodBox
{

ContactInfoData::ContactInfoData() : QSharedData()
{
    this->userId = 0;
    this->status = UserStatus::Undefined;
    this->authorizationState = AuthorizationState::Undefined;
    this->isBlocked = false;
    this->type = ContactType::Undefined;
}
ContactInfoData::ContactInfoData(qint32 userId, QString login, UserStatus::UserStatusEnum status, QString motto, QString name, QDate birthDay, AuthorizationState::AuthorizationStateEnum authorizationState, QString message, bool isBlocked, ContactType::ContactTypeEnum type) : QSharedData()
{
    this->userId = userId;
    this->login = login;
    this->status = status;
    this->motto = motto;
    this->name = name;
    this->birthDay = birthDay;
    this->authorizationState = authorizationState;
    this->message = message;
    this->isBlocked = isBlocked;
    this->type = type;
}

ContactInfoData::~ContactInfoData()
{
}

ContactInfoIntermediate::ContactInfoIntermediate() : TransportableObject()
{
}
ContactInfoIntermediate::ContactInfoIntermediate(qint32 userId, QString login, UserStatus::UserStatusEnum status, QString motto, QString name, QDate birthDay, AuthorizationState::AuthorizationStateEnum authorizationState, QString message, bool isBlocked, ContactType::ContactTypeEnum type) : TransportableObject()
{
    d = new ContactInfoData(userId, login, status, motto, name, birthDay, authorizationState, message, isBlocked, type);
}

ContactInfoIntermediate::~ContactInfoIntermediate()
{
}

qint32 ContactInfoIntermediate::getUserId() const
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::getUserId", "Getter call on object which isNull");
    return this->d->userId;
}
void ContactInfoIntermediate::setUserId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::setUserId", "Setter call on object which isNull");
    this->d->userId = value;
}
QString ContactInfoIntermediate::getLogin() const
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::getLogin", "Getter call on object which isNull");
    return this->d->login;
}
void ContactInfoIntermediate::setLogin(QString value)
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::setLogin", "Setter call on object which isNull");
    this->d->login = value;
}
UserStatus::UserStatusEnum ContactInfoIntermediate::getStatus() const
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::getStatus", "Getter call on object which isNull");
    return this->d->status;
}
void ContactInfoIntermediate::setStatus(UserStatus::UserStatusEnum value)
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::setStatus", "Setter call on object which isNull");
    this->d->status = value;
}
QString ContactInfoIntermediate::getMotto() const
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::getMotto", "Getter call on object which isNull");
    return this->d->motto;
}
void ContactInfoIntermediate::setMotto(QString value)
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::setMotto", "Setter call on object which isNull");
    this->d->motto = value;
}
QString ContactInfoIntermediate::getName() const
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::getName", "Getter call on object which isNull");
    return this->d->name;
}
void ContactInfoIntermediate::setName(QString value)
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::setName", "Setter call on object which isNull");
    this->d->name = value;
}
QDate ContactInfoIntermediate::getBirthDay() const
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::getBirthDay", "Getter call on object which isNull");
    return this->d->birthDay;
}
void ContactInfoIntermediate::setBirthDay(QDate value)
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::setBirthDay", "Setter call on object which isNull");
    this->d->birthDay = value;
}
AuthorizationState::AuthorizationStateEnum ContactInfoIntermediate::getAuthorizationState() const
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::getAuthorizationState", "Getter call on object which isNull");
    return this->d->authorizationState;
}
void ContactInfoIntermediate::setAuthorizationState(AuthorizationState::AuthorizationStateEnum value)
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::setAuthorizationState", "Setter call on object which isNull");
    this->d->authorizationState = value;
}
QString ContactInfoIntermediate::getMessage() const
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::getMessage", "Getter call on object which isNull");
    return this->d->message;
}
void ContactInfoIntermediate::setMessage(QString value)
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::setMessage", "Setter call on object which isNull");
    this->d->message = value;
}
bool ContactInfoIntermediate::getIsBlocked() const
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::getIsBlocked", "Getter call on object which isNull");
    return this->d->isBlocked;
}
void ContactInfoIntermediate::setIsBlocked(bool value)
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::setIsBlocked", "Setter call on object which isNull");
    this->d->isBlocked = value;
}
ContactType::ContactTypeEnum ContactInfoIntermediate::getType() const
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::getType", "Getter call on object which isNull");
    return this->d->type;
}
void ContactInfoIntermediate::setType(ContactType::ContactTypeEnum value)
{
    Q_ASSERT_X(!isNull(), "ContactInfoIntermediate::setType", "Setter call on object which isNull");
    this->d->type = value;
}

qint32 ContactInfoIntermediate::getRepresentedTypeId()
{
    return 6;
}

qint32 ContactInfoIntermediate::getTypeId() const
{
    return 6;
}
void ContactInfoIntermediate::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->userId);
    writer->writeProperty(this, 2, this->d->login);
    writer->writeEnumProperty(this, 3, 20015, this->d->status);
    writer->writeProperty(this, 4, this->d->motto);
    writer->writeProperty(this, 5, this->d->name);
    writer->writeProperty(this, 6, this->d->birthDay);
    writer->writeEnumProperty(this, 7, 20008, this->d->authorizationState);
    writer->writeProperty(this, 8, this->d->message);
    writer->writeProperty(this, 9, this->d->isBlocked);
    writer->writeEnumProperty(this, 10, 20023, this->d->type);
}
PropertyReadResult ContactInfoIntermediate::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
            this->d->login = reader->readString();
            return PropertyReadResult(true);
        case 3:
            this->d->status = (UserStatus::UserStatusEnum)reader->readEnum(20015);
            return PropertyReadResult(true);
        case 4:
            this->d->motto = reader->readString();
            return PropertyReadResult(true);
        case 5:
            this->d->name = reader->readString();
            return PropertyReadResult(true);
        case 6:
            this->d->birthDay = reader->readDate();
            return PropertyReadResult(true);
        case 7:
            this->d->authorizationState = (AuthorizationState::AuthorizationStateEnum)reader->readEnum(20008);
            return PropertyReadResult(true);
        case 8:
            this->d->message = reader->readString();
            return PropertyReadResult(true);
        case 9:
            this->d->isBlocked = reader->readBool();
            return PropertyReadResult(true);
        case 10:
            this->d->type = (ContactType::ContactTypeEnum)reader->readEnum(20023);
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
