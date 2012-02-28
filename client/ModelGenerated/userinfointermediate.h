#ifndef USERINFOINTERMEDIATE_H
#define USERINFOINTERMEDIATE_H

#include <QSharedData>
#include <QLocale>
#include <QDateTime>
#include <QString>
#include <QDate>

#include "userstatus.h"
#include "transportableobject.h"
#include "sex.h"
#include "country.h"

namespace MoodBox
{

class UserInfoData : public QSharedData
{
public:
    UserInfoData();
    UserInfoData(qint32 userId, QString login, QString name, QDateTime creationDate, QString motto, QString aboutMe, QLocale::Country country, QString city, Sex::SexEnum sex, QDate birthDay, QString userpicUrl, UserStatus::UserStatusEnum status, bool showTv);
    virtual ~UserInfoData();

    qint32 userId;
    QString login;
    QString name;
    QDateTime creationDate;
    QString motto;
    QString aboutMe;
    QLocale::Country country;
    QString city;
    Sex::SexEnum sex;
    QDate birthDay;
    QString userpicUrl;
    UserStatus::UserStatusEnum status;
    bool showTv;
};

class UserInfoIntermediate : public TransportableObject
{
protected:
    UserInfoIntermediate();
    UserInfoIntermediate(qint32 userId, QString login, QString name, QDateTime creationDate, QString motto, QString aboutMe, QLocale::Country country, QString city, Sex::SexEnum sex, QDate birthDay, QString userpicUrl, UserStatus::UserStatusEnum status, bool showTv);
public:
    virtual ~UserInfoIntermediate();

protected:
    UserInfoIntermediate(UserInfoData* dataRef)
    {
        this->d = dataRef;
    }
public:
    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getUserId() const;
    void setUserId(qint32 value);
    QString getLogin() const;
    void setLogin(QString value);
    QString getName() const;
    void setName(QString value);
    QDateTime getCreationDate() const;
    void setCreationDate(QDateTime value);
    QString getMotto() const;
    void setMotto(QString value);
    QString getAboutMe() const;
    void setAboutMe(QString value);
    QLocale::Country getCountry() const;
    void setCountry(QLocale::Country value);
    QString getCity() const;
    void setCity(QString value);
    Sex::SexEnum getSex() const;
    void setSex(Sex::SexEnum value);
    QDate getBirthDay() const;
    void setBirthDay(QDate value);
    QString getUserpicUrl() const;
    void setUserpicUrl(QString value);
    UserStatus::UserStatusEnum getStatus() const;
    void setStatus(UserStatus::UserStatusEnum value);
    bool getShowTv() const;
    void setShowTv(bool value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<UserInfoData> d;
};

}

#endif // USERINFOINTERMEDIATE_H

// template of a real class

/**************************************************

#ifndef USERINFO_H
#define USERINFO_H

#include "userinfointermediate.h"

namespace MoodBox
{

class UserInfo : public UserInfoIntermediate
{
public:
    UserInfo() : UserInfoIntermediate()
    {
    }
    UserInfo(qint32 userId, QString login, QString name, QDateTime creationDate, QString motto, QString aboutMe, QLocale::Country country, QString city, Sex::SexEnum sex, QDate birthDay, QString userpicUrl, UserStatus::UserStatusEnum status, bool showTv) : UserInfoIntermediate(userId, login, name, creationDate, motto, aboutMe, country, city, sex, birthDay, userpicUrl, status, showTv)
    {
    }
    virtual ~UserInfo()
    {
    }

protected:
    UserInfo(UserInfoData* dataRef) : UserInfoIntermediate(dataRef)
    {
    }

public:
    // never use ___new_ in your code!!!
    static UserInfo* ___new_()
    {
        return new UserInfo(new UserInfoData());
    }
    static UserInfo empty()
    {
        return UserInfo(new UserInfoData());
    }

    // below are added methods



};

}

#endif // USERINFO_H

**************************************************/
