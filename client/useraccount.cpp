#include "useraccount.h"

namespace MoodBox
{

QString UserAccount::getDisplayName() const
{
	QString name = getName().trimmed();

	return (!name.isEmpty()) ? name : getLogin();
}

}
