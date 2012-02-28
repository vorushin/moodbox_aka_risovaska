#include "userinfo.h"

namespace MoodBox
{

QString UserInfo::getDisplayName() const
{
	QString name = getName();

	return (!name.isEmpty()) ? name : getLogin();
}

int UserInfo::getAge() const
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

}
