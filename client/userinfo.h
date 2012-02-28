#ifndef USERINFO_H
#define USERINFO_H

#include <QMetaType>

#include "userinfointermediate.h"

namespace MoodBox
{

class UserInfo : public UserInfoIntermediate
{
public:
    UserInfo() : UserInfoIntermediate()
    {
    }
    UserInfo(qint32 userId, QString login, QString name, QDateTime creationDate, QString motto, QString aboutMe, QLocale::Country country, QString city, Sex::SexEnum sex, QDate birthDay, QString userpicUrl, UserStatus::UserStatusEnum status) : UserInfoIntermediate(userId, login, name, creationDate, motto, aboutMe, country, city, sex, birthDay, userpicUrl, status, false)
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

	// Universal name function
	QString getDisplayName() const;

	// User age
	int getAge() const;

};

}

Q_DECLARE_METATYPE(MoodBox::UserInfo)

#endif // USERINFO_H
