#include "flagscollection.h"

namespace MoodBox
{

// PendingRequestsCollection class
bool FlagsCollection::getFlag(qint32 id, qint32 flag) const
{
	if (!contains(id))
		return false;

	return value(id) & flag;
}

void FlagsCollection::setFlag(qint32 id, qint32 flag)
{
	qint32 state = 0;

	// if there is something about this contact
	if (contains(id))
		state = value(id);

	// add a new flag
	state |= flag;
	insert(id, state);
}

void FlagsCollection::clearFlag(qint32 id, qint32 flag)
{
	if (!contains(id))
		return;

	qint32 state = value(id);

	state ^= flag;
	if (state == 0)
		remove(id);
	else
		insert(id, state);
}

void FlagsCollection::clear()
{
	QMap<qint32, qint32>::clear();
}

}