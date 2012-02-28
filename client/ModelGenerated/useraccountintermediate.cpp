#include "listwrapperobjects.h"
#include "useraccountintermediate.h"

namespace MoodBox
{

UserAccountData::UserAccountData() : QSharedData()
{
    this->id = 0;
    this->country = QLocale::AnyCountry;
    this->sex = Sex::Undefined;
    this->language = Language::Undefined;
    this->allowNews = false;
    this->allowPublishing = false;
    this->allowShowFriends = false;
    this->role = Role::Undefined;
}
UserAccountData::UserAccountData(qint32 id, QString login, QString password, QDateTime creationDate, QString motto, QString aboutMe, QString name, QLocale::Country country, QString city, QString email, Sex::SexEnum sex, QDate birthDay, Language::LanguageEnum language, bool allowNews, bool allowPublishing, bool allowShowFriends, QString userpicUrl, Role::RoleEnum role, QList<qint32> moderateContacts) : QSharedData()
{
    this->id = id;
    this->login = login;
    this->password = password;
    this->creationDate = creationDate;
    this->motto = motto;
    this->aboutMe = aboutMe;
    this->name = name;
    this->country = country;
    this->city = city;
    this->email = email;
    this->sex = sex;
    this->birthDay = birthDay;
    this->language = language;
    this->allowNews = allowNews;
    this->allowPublishing = allowPublishing;
    this->allowShowFriends = allowShowFriends;
    this->userpicUrl = userpicUrl;
    this->role = role;
    this->moderateContacts = moderateContacts;
}

UserAccountData::~UserAccountData()
{
}

UserAccountIntermediate::UserAccountIntermediate() : TransportableObject()
{
}
UserAccountIntermediate::UserAccountIntermediate(qint32 id, QString login, QString password, QDateTime creationDate, QString motto, QString aboutMe, QString name, QLocale::Country country, QString city, QString email, Sex::SexEnum sex, QDate birthDay, Language::LanguageEnum language, bool allowNews, bool allowPublishing, bool allowShowFriends, QString userpicUrl, Role::RoleEnum role, QList<qint32> moderateContacts) : TransportableObject()
{
    d = new UserAccountData(id, login, password, creationDate, motto, aboutMe, name, country, city, email, sex, birthDay, language, allowNews, allowPublishing, allowShowFriends, userpicUrl, role, moderateContacts);
}

UserAccountIntermediate::~UserAccountIntermediate()
{
}

qint32 UserAccountIntermediate::getId() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getId", "Getter call on object which isNull");
    return this->d->id;
}
void UserAccountIntermediate::setId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setId", "Setter call on object which isNull");
    this->d->id = value;
}
QString UserAccountIntermediate::getLogin() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getLogin", "Getter call on object which isNull");
    return this->d->login;
}
void UserAccountIntermediate::setLogin(QString value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setLogin", "Setter call on object which isNull");
    this->d->login = value;
}
QString UserAccountIntermediate::getPassword() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getPassword", "Getter call on object which isNull");
    return this->d->password;
}
void UserAccountIntermediate::setPassword(QString value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setPassword", "Setter call on object which isNull");
    this->d->password = value;
}
QDateTime UserAccountIntermediate::getCreationDate() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getCreationDate", "Getter call on object which isNull");
    return this->d->creationDate;
}
void UserAccountIntermediate::setCreationDate(QDateTime value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setCreationDate", "Setter call on object which isNull");
    this->d->creationDate = value;
}
QString UserAccountIntermediate::getMotto() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getMotto", "Getter call on object which isNull");
    return this->d->motto;
}
void UserAccountIntermediate::setMotto(QString value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setMotto", "Setter call on object which isNull");
    this->d->motto = value;
}
QString UserAccountIntermediate::getAboutMe() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getAboutMe", "Getter call on object which isNull");
    return this->d->aboutMe;
}
void UserAccountIntermediate::setAboutMe(QString value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setAboutMe", "Setter call on object which isNull");
    this->d->aboutMe = value;
}
QString UserAccountIntermediate::getName() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getName", "Getter call on object which isNull");
    return this->d->name;
}
void UserAccountIntermediate::setName(QString value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setName", "Setter call on object which isNull");
    this->d->name = value;
}
QLocale::Country UserAccountIntermediate::getCountry() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getCountry", "Getter call on object which isNull");
    return this->d->country;
}
void UserAccountIntermediate::setCountry(QLocale::Country value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setCountry", "Setter call on object which isNull");
    this->d->country = value;
}
QString UserAccountIntermediate::getCity() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getCity", "Getter call on object which isNull");
    return this->d->city;
}
void UserAccountIntermediate::setCity(QString value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setCity", "Setter call on object which isNull");
    this->d->city = value;
}
QString UserAccountIntermediate::getEmail() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getEmail", "Getter call on object which isNull");
    return this->d->email;
}
void UserAccountIntermediate::setEmail(QString value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setEmail", "Setter call on object which isNull");
    this->d->email = value;
}
Sex::SexEnum UserAccountIntermediate::getSex() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getSex", "Getter call on object which isNull");
    return this->d->sex;
}
void UserAccountIntermediate::setSex(Sex::SexEnum value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setSex", "Setter call on object which isNull");
    this->d->sex = value;
}
QDate UserAccountIntermediate::getBirthDay() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getBirthDay", "Getter call on object which isNull");
    return this->d->birthDay;
}
void UserAccountIntermediate::setBirthDay(QDate value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setBirthDay", "Setter call on object which isNull");
    this->d->birthDay = value;
}
Language::LanguageEnum UserAccountIntermediate::getLanguage() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getLanguage", "Getter call on object which isNull");
    return this->d->language;
}
void UserAccountIntermediate::setLanguage(Language::LanguageEnum value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setLanguage", "Setter call on object which isNull");
    this->d->language = value;
}
bool UserAccountIntermediate::getAllowNews() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getAllowNews", "Getter call on object which isNull");
    return this->d->allowNews;
}
void UserAccountIntermediate::setAllowNews(bool value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setAllowNews", "Setter call on object which isNull");
    this->d->allowNews = value;
}
bool UserAccountIntermediate::getAllowPublishing() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getAllowPublishing", "Getter call on object which isNull");
    return this->d->allowPublishing;
}
void UserAccountIntermediate::setAllowPublishing(bool value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setAllowPublishing", "Setter call on object which isNull");
    this->d->allowPublishing = value;
}
bool UserAccountIntermediate::getAllowShowFriends() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getAllowShowFriends", "Getter call on object which isNull");
    return this->d->allowShowFriends;
}
void UserAccountIntermediate::setAllowShowFriends(bool value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setAllowShowFriends", "Setter call on object which isNull");
    this->d->allowShowFriends = value;
}
QString UserAccountIntermediate::getUserpicUrl() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getUserpicUrl", "Getter call on object which isNull");
    return this->d->userpicUrl;
}
void UserAccountIntermediate::setUserpicUrl(QString value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setUserpicUrl", "Setter call on object which isNull");
    this->d->userpicUrl = value;
}
Role::RoleEnum UserAccountIntermediate::getRole() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getRole", "Getter call on object which isNull");
    return this->d->role;
}
void UserAccountIntermediate::setRole(Role::RoleEnum value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setRole", "Setter call on object which isNull");
    this->d->role = value;
}
QList<qint32> UserAccountIntermediate::getModerateContacts() const
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::getModerateContacts", "Getter call on object which isNull");
    return this->d->moderateContacts;
}
void UserAccountIntermediate::setModerateContacts(QList<qint32> value)
{
    Q_ASSERT_X(!isNull(), "UserAccountIntermediate::setModerateContacts", "Setter call on object which isNull");
    this->d->moderateContacts = value;
}

qint32 UserAccountIntermediate::getRepresentedTypeId()
{
    return 4;
}

qint32 UserAccountIntermediate::getTypeId() const
{
    return 4;
}
void UserAccountIntermediate::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->id);
    writer->writeProperty(this, 2, this->d->login);
    writer->writeProperty(this, 3, this->d->password);
    writer->writeProperty(this, 4, this->d->creationDate);
    writer->writeProperty(this, 5, this->d->motto);
    writer->writeProperty(this, 6, this->d->aboutMe);
    writer->writeProperty(this, 7, this->d->name);
    writer->writeEnumProperty(this, 8, 20002, this->d->country);
    writer->writeProperty(this, 9, this->d->city);
    writer->writeProperty(this, 10, this->d->email);
    writer->writeEnumProperty(this, 11, 20001, this->d->sex);
    writer->writeProperty(this, 12, this->d->birthDay);
    writer->writeEnumProperty(this, 13, 20003, this->d->language);
    writer->writeProperty(this, 14, this->d->allowNews);
    writer->writeProperty(this, 15, this->d->allowPublishing);
    writer->writeProperty(this, 16, this->d->allowShowFriends);
    writer->writeProperty(this, 17, this->d->userpicUrl);
    writer->writeEnumProperty(this, 18, 20027, this->d->role);
    writer->writeProperty(this, 19, this->d->moderateContacts);
}
PropertyReadResult UserAccountIntermediate::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->id = reader->readInt32();
            return PropertyReadResult(true);
        case 2:
            this->d->login = reader->readString();
            return PropertyReadResult(true);
        case 3:
            this->d->password = reader->readString();
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
            this->d->name = reader->readString();
            return PropertyReadResult(true);
        case 8:
            this->d->country = (QLocale::Country)reader->readEnum(20002);
            return PropertyReadResult(true);
        case 9:
            this->d->city = reader->readString();
            return PropertyReadResult(true);
        case 10:
            this->d->email = reader->readString();
            return PropertyReadResult(true);
        case 11:
            this->d->sex = (Sex::SexEnum)reader->readEnum(20001);
            return PropertyReadResult(true);
        case 12:
            this->d->birthDay = reader->readDate();
            return PropertyReadResult(true);
        case 13:
            this->d->language = (Language::LanguageEnum)reader->readEnum(20003);
            return PropertyReadResult(true);
        case 14:
            this->d->allowNews = reader->readBool();
            return PropertyReadResult(true);
        case 15:
            this->d->allowPublishing = reader->readBool();
            return PropertyReadResult(true);
        case 16:
            this->d->allowShowFriends = reader->readBool();
            return PropertyReadResult(true);
        case 17:
            this->d->userpicUrl = reader->readString();
            return PropertyReadResult(true);
        case 18:
            this->d->role = (Role::RoleEnum)reader->readEnum(20027);
            return PropertyReadResult(true);
        case 19:
            this->d->moderateContacts = QList<qint32>();
            return PropertyReadResult(new ListOfInt32WrapperObject(&this->d->moderateContacts, PropertyInfo(false, false)));
    }

    return PropertyReadResult(false);
}

}
