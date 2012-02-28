#ifndef USERACCOUNTINTERMEDIATE_H
#define USERACCOUNTINTERMEDIATE_H

#include <QSharedData>
#include <QLocale>
#include <QList>
#include <QDateTime>
#include <QString>
#include <QDate>

#include "transportableobject.h"
#include "language.h"
#include "role.h"
#include "sex.h"
#include "country.h"

namespace MoodBox
{

class UserAccountData : public QSharedData
{
public:
    UserAccountData();
    UserAccountData(qint32 id, QString login, QString password, QDateTime creationDate, QString motto, QString aboutMe, QString name, QLocale::Country country, QString city, QString email, Sex::SexEnum sex, QDate birthDay, Language::LanguageEnum language, bool allowNews, bool allowPublishing, bool allowShowFriends, QString userpicUrl, Role::RoleEnum role, QList<qint32> moderateContacts);
    virtual ~UserAccountData();

    qint32 id;
    QString login;
    QString password;
    QDateTime creationDate;
    QString motto;
    QString aboutMe;
    QString name;
    QLocale::Country country;
    QString city;
    QString email;
    Sex::SexEnum sex;
    QDate birthDay;
    Language::LanguageEnum language;
    bool allowNews;
    bool allowPublishing;
    bool allowShowFriends;
    QString userpicUrl;
    Role::RoleEnum role;
    QList<qint32> moderateContacts;
};

class UserAccountIntermediate : public TransportableObject
{
protected:
    UserAccountIntermediate();
    UserAccountIntermediate(qint32 id, QString login, QString password, QDateTime creationDate, QString motto, QString aboutMe, QString name, QLocale::Country country, QString city, QString email, Sex::SexEnum sex, QDate birthDay, Language::LanguageEnum language, bool allowNews, bool allowPublishing, bool allowShowFriends, QString userpicUrl, Role::RoleEnum role, QList<qint32> moderateContacts);
public:
    virtual ~UserAccountIntermediate();

protected:
    UserAccountIntermediate(UserAccountData* dataRef)
    {
        this->d = dataRef;
    }
public:
    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getId() const;
    void setId(qint32 value);
    QString getLogin() const;
    void setLogin(QString value);
    QString getPassword() const;
    void setPassword(QString value);
    QDateTime getCreationDate() const;
    void setCreationDate(QDateTime value);
    QString getMotto() const;
    void setMotto(QString value);
    QString getAboutMe() const;
    void setAboutMe(QString value);
    QString getName() const;
    void setName(QString value);
    QLocale::Country getCountry() const;
    void setCountry(QLocale::Country value);
    QString getCity() const;
    void setCity(QString value);
    QString getEmail() const;
    void setEmail(QString value);
    Sex::SexEnum getSex() const;
    void setSex(Sex::SexEnum value);
    QDate getBirthDay() const;
    void setBirthDay(QDate value);
    Language::LanguageEnum getLanguage() const;
    void setLanguage(Language::LanguageEnum value);
    bool getAllowNews() const;
    void setAllowNews(bool value);
    bool getAllowPublishing() const;
    void setAllowPublishing(bool value);
    bool getAllowShowFriends() const;
    void setAllowShowFriends(bool value);
    QString getUserpicUrl() const;
    void setUserpicUrl(QString value);
    Role::RoleEnum getRole() const;
    void setRole(Role::RoleEnum value);
    QList<qint32> getModerateContacts() const;
    void setModerateContacts(QList<qint32> value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<UserAccountData> d;
};

}

#endif // USERACCOUNTINTERMEDIATE_H

// template of a real class

/**************************************************

#ifndef USERACCOUNT_H
#define USERACCOUNT_H

#include "useraccountintermediate.h"

namespace MoodBox
{

class UserAccount : public UserAccountIntermediate
{
public:
    UserAccount() : UserAccountIntermediate()
    {
    }
    UserAccount(qint32 id, QString login, QString password, QDateTime creationDate, QString motto, QString aboutMe, QString name, QLocale::Country country, QString city, QString email, Sex::SexEnum sex, QDate birthDay, Language::LanguageEnum language, bool allowNews, bool allowPublishing, bool allowShowFriends, QString userpicUrl, Role::RoleEnum role, QList<qint32> moderateContacts) : UserAccountIntermediate(id, login, password, creationDate, motto, aboutMe, name, country, city, email, sex, birthDay, language, allowNews, allowPublishing, allowShowFriends, userpicUrl, role, moderateContacts)
    {
    }
    virtual ~UserAccount()
    {
    }

protected:
    UserAccount(UserAccountData* dataRef) : UserAccountIntermediate(dataRef)
    {
    }

public:
    // never use ___new_ in your code!!!
    static UserAccount* ___new_()
    {
        return new UserAccount(new UserAccountData());
    }
    static UserAccount empty()
    {
        return UserAccount(new UserAccountData());
    }

    // below are added methods



};

}

#endif // USERACCOUNT_H

**************************************************/
