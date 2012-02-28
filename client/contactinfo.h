#ifndef CONTACTINFO_H
#define CONTACTINFO_H

#include "contactinfointermediate.h"

namespace MoodBox
{

// Class for user's contact information
class ContactInfo : public ContactInfoIntermediate
{
public:
	enum SortGroup { AllFriends = 0, Channels = 1, MySandbox = 2, Online = 3, WaitsAuthorizationFromMe = 4, Offline = 5, IWaitAuthorization = 6 };

public:
    ContactInfo() : ContactInfoIntermediate()
    {
    }
    ContactInfo(qint32 userId, QString login, UserStatus::UserStatusEnum status, QString motto, QString name, QDate birthDay, AuthorizationState::AuthorizationStateEnum authorizationState, QString message, bool isBlocked, ContactType::ContactTypeEnum type) : ContactInfoIntermediate(userId, login, status, motto, name, birthDay, authorizationState, message, isBlocked, type)
    {
    }
    virtual ~ContactInfo()
    {
    }

protected:
    ContactInfo(ContactInfoData* dataRef) : ContactInfoIntermediate(dataRef)
    {
    }

public:
    // never use ___new_ in your code!!!
    inline static ContactInfo* ___new_() 
	{ 
		return new ContactInfo(new ContactInfoData()); 
	}

    inline static ContactInfo empty() 
	{ 
		return ContactInfo(new ContactInfoData()); 
	}

	static ContactInfo allFriendsContact();

	static ContactInfo mySandboxContact();

	// Universal name function
	QString getDisplayName() const;

	// User age
	int getAge() const;

	// Sort group
	SortGroup getSortGroup() const;

	// is all friends?
	bool isAllFriendsContact () const;

	// Comparison
	bool operator < (const ContactInfo &other) const;

};

}

Q_DECLARE_METATYPE(MoodBox::ContactInfo)

#endif // CONTACTINFO_H
