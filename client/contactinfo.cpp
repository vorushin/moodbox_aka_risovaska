#include "contactinfo.h"

#include "newcontactlistwindow.h"
#include "peopleinfomanager.h"

namespace MoodBox
{

ContactInfo ContactInfo::allFriendsContact()
{
	return ContactInfo(-1, QString(), UserStatus::Online, QString(), QString(), QDate(), AuthorizationState::Authorized, QString(), false, ContactType::Channel);
}

ContactInfo ContactInfo::mySandboxContact()
{
	return ContactInfo(INFOMANAGER->getUserAccount().getId(), QString(), UserStatus::Online, QString(), QString(), 
		QDate(), AuthorizationState::Authorized, QString(), false, ContactType::Undefined);
}

QString ContactInfo::getDisplayName() const
{
	QString name = getName().trimmed();

	return (!name.isEmpty()) ? name : getLogin();
}

int ContactInfo::getAge() const
{
	if (!getBirthDay().isValid())
		return -1;

	int age = QDate::currentDate().year() - getBirthDay().year();
	if (QDate::currentDate().month() < getBirthDay().month() // current month is less than BD month
		|| (QDate::currentDate().month() == getBirthDay().month() 
			&& QDate::currentDate().day() < getBirthDay().day())) // month is the same, but day is less than BD day
	{
		age--;
	}

	return age;
}

ContactInfo::SortGroup ContactInfo::getSortGroup() const
{
	if (getUserId() == INFOMANAGER->getUserAccount().getId())
		return MySandbox;

	if (isAllFriendsContact())
		return AllFriends;

	if (getType() == ContactType::Channel)
		return Channels;

	if (getAuthorizationState() == AuthorizationState::WaitsAuthorizationFromMe)
		return WaitsAuthorizationFromMe;

	if (getAuthorizationState() == AuthorizationState::NotAuthorizedMe)
		return IWaitAuthorization;

	return (getStatus() == UserStatus::Online) ? Online : Offline;
}

bool ContactInfo::isAllFriendsContact () const
{
	return getUserId() == -1;
}

bool ContactInfo::operator < (const ContactInfo &other) const
{
	if (getSortGroup() == other.getSortGroup())
		return getDisplayName().toUpper() < other.getDisplayName().toUpper();

	return getSortGroup() < other.getSortGroup();
}

}