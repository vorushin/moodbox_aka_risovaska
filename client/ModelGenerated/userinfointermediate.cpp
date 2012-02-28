#include "listwrapperobjects.h"
#include "userinfointermediate.h"

namespace MoodBox
{

UserInfoData::UserInfoData() : QSharedData()
{
    this->userId = 0;
    this->country = QLocale::AnyCountry;
    this->sex = Sex::Undefined;
    this->status = UserStatus::Undefined;
    this->showTv = false;
}
UserInfoData::UserInfoData(qint32 userId, QString login, QString name, QDateTime creationDate, QString motto, QString aboutMe, QLocale::Country country, QString city, Sex::SexEnum sex, QDate birthDay, QString userpicUrl, UserStatus::UserStatusEnum status, bool showTv) : QSharedData()
{
    this->userId = userId;
    this->login = login;
    this->name = name;
    this->creationDate = creationDate;
    this->motto = motto;
    this->aboutMe = aboutMe;
    this->country = country;
    this->city = city;
    this->sex = sex;
    this->birthDay = birthDay;
    this->userpicUrl = userpicUrl;
    this->status = status;
    this->showTv = showTv;
}

UserInfoData::~UserInfoData()
{
}

UserInfoIntermediate::UserInfoIntermediate() : TransportableObject()
{
}
UserInfoIntermediate::UserInfoIntermediate(qint32 userId, QString login, QString name, QDateTime creationDate, QString motto, QString aboutMe, QLocale::Country country, QString city, Sex::SexEnum sex, QDate birthDay, QString userpicUrl, UserStatus::UserStatusEnum status, bool showTv) : TransportableObject()
{
    d = new UserInfoData(userId, login, name, creationDate, motto, aboutMe, country, city, sex, birthDay, userpicUrl, status, showTv);
}

UserInfoIntermediate::~UserInfoIntermediate()
{
}

qint32 UserInfoIntermediate::getUserId() const
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::getUserId", "Getter call on object which isNull");
    return this->d->userId;
}
void UserInfoIntermediate::setUserId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::setUserId", "Setter call on object which isNull");
    this->d->userId = value;
}
QString UserInfoIntermediate::getLogin() const
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::getLogin", "Getter call on object which isNull");
    return this->d->login;
}
void UserInfoIntermediate::setLogin(QString value)
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::setLogin", "Setter call on object which isNull");
    this->d->login = value;
}
QString UserInfoIntermediate::getName() const
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::getName", "Getter call on object which isNull");
    return this->d->name;
}
void UserInfoIntermediate::setName(QString value)
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::setName", "Setter call on object which isNull");
    this->d->name = value;
}
QDateTime UserInfoIntermediate::getCreationDate() const
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::getCreationDate", "Getter call on object which isNull");
    return this->d->creationDate;
}
void UserInfoIntermediate::setCreationDate(QDateTime value)
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::setCreationDate", "Setter call on object which isNull");
    this->d->creationDate = value;
}
QString UserInfoIntermediate::getMotto() const
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::getMotto", "Getter call on object which isNull");
    return this->d->motto;
}
void UserInfoIntermediate::setMotto(QString value)
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::setMotto", "Setter call on object which isNull");
    this->d->motto = value;
}
QString UserInfoIntermediate::getAboutMe() const
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::getAboutMe", "Getter call on object which isNull");
    return this->d->aboutMe;
}
void UserInfoIntermediate::setAboutMe(QString value)
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::setAboutMe", "Setter call on object which isNull");
    this->d->aboutMe = value;
}
QLocale::Country UserInfoIntermediate::getCountry() const
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::getCountry", "Getter call on object which isNull");
    return this->d->country;
}
void UserInfoIntermediate::setCountry(QLocale::Country value)
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::setCountry", "Setter call on object which isNull");
    this->d->country = value;
}
QString UserInfoIntermediate::getCity() const
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::getCity", "Getter call on object which isNull");
    return this->d->city;
}
void UserInfoIntermediate::setCity(QString value)
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::setCity", "Setter call on object which isNull");
    this->d->city = value;
}
Sex::SexEnum UserInfoIntermediate::getSex() const
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::getSex", "Getter call on object which isNull");
    return this->d->sex;
}
void UserInfoIntermediate::setSex(Sex::SexEnum value)
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::setSex", "Setter call on object which isNull");
    this->d->sex = value;
}
QDate UserInfoIntermediate::getBirthDay() const
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::getBirthDay", "Getter call on object which isNull");
    return this->d->birthDay;
}
void UserInfoIntermediate::setBirthDay(QDate value)
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::setBirthDay", "Setter call on object which isNull");
    this->d->birthDay = value;
}
QString UserInfoIntermediate::getUserpicUrl() const
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::getUserpicUrl", "Getter call on object which isNull");
    return this->d->userpicUrl;
}
void UserInfoIntermediate::setUserpicUrl(QString value)
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::setUserpicUrl", "Setter call on object which isNull");
    this->d->userpicUrl = value;
}
UserStatus::UserStatusEnum UserInfoIntermediate::getStatus() const
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::getStatus", "Getter call on object which isNull");
    return this->d->status;
}
void UserInfoIntermediate::setStatus(UserStatus::UserStatusEnum value)
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::setStatus", "Setter call on object which isNull");
    this->d->status = value;
}
bool UserInfoIntermediate::getShowTv() const
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::getShowTv", "Getter call on object which isNull");
    return this->d->showTv;
}
void UserInfoIntermediate::setShowTv(bool value)
{
    Q_ASSERT_X(!isNull(), "UserInfoIntermediate::setShowTv", "Setter call on object which isNull");
    this->d->showTv = value;
}

qint32 UserInfoIntermediate::getRepresentedTypeId()
{
    return 8;
}

qint32 UserInfoIntermediate::getTypeId() const
{
    return 8;
}
void UserInfoIntermediate::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->userId);
    writer->writeProperty(this, 2, this->d->login);
    writer->writeProperty(this, 3, this->d->name);
    writer->writeProperty(this, 4, this->d->creationDate);
    writer->writeProperty(this, 5, this->d->motto);
    writer->writeProperty(this, 6, this->d->aboutMe);
    writer->writeEnumProperty(this, 7, 20002, this->d->country);
    writer->writeProperty(this, 8, this->d->city);
    writer->writeEnumProperty(this, 9, 20001, this->d->sex);
    writer->writeProperty(this, 10, this->d->birthDay);
    writer->writeProperty(this, 11, this->d->userpicUrl);
    writer->writeEnumProperty(this, 12, 20015, this->d->status);
    writer->writeProperty(this, 13, this->d->showTv);
}
PropertyReadResult UserInfoIntermediate::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
            this->d->name = reader->readString();
            return PropertyReadResult(true);
        case 4:
            this->d->creationDate = reader->readDateTime();
            return PropertyReadResult(true);
        case 5:
            this->d->motto = reader->readString();
            return PropertyReadResult(true);
        case 6:
            this->d->aboutMe = reader->readString();
            return PropertyReadResult(true);
        case 7:
            this->d->country = (QLocale::Country)reader->readEnum(20002);
            return PropertyReadResult(true);
        case 8:
            this->d->city = reader->readString();
            return PropertyReadResult(true);
        case 9:
            this->d->sex = (Sex::SexEnum)reader->readEnum(20001);
            return PropertyReadResult(true);
        case 10:
            this->d->birthDay = reader->readDate();
            return PropertyReadResult(true);
        case 11:
            this->d->userpicUrl = reader->readString();
            return PropertyReadResult(true);
        case 12:
            this->d->status = (UserStatus::UserStatusEnum)reader->readEnum(20015);
            return PropertyReadResult(true);
        case 13:
            this->d->showTv = reader->readBool();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
