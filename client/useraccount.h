#ifndef USERACCOUNT_H
#define USERACCOUNT_H

#include <QMetaType>

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

   	// Universal name function
	QString getDisplayName() const;

};

}

Q_DECLARE_METATYPE(MoodBox::UserAccount)

#endif // USERACCOUNT_H
