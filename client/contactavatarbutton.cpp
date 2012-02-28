#include "contactavatarbutton.h"

#include "peopleinfomanager.h"

namespace MoodBox
{

ContactAvatarButton::ContactAvatarButton(QWidget *parent)
	: AvatarButton(parent), contactId(-1)
{
}

void ContactAvatarButton::setContactId(qint32 contactId)
{
	this->contactId = contactId;

	connect(INFOMANAGER, SIGNAL(contactPictureChanged(qint32)), this, SLOT(onContactPictureChanged(qint32)));

	// Update avatar's cache
	updateCache();
}

void ContactAvatarButton::setPicture(const QPixmap &picture)
{
	pixmap = picture;
	updateCache();
}

QPixmap ContactAvatarButton::getSourcePicture(bool thumbnail)
{
	QPixmap picture;

	if (contactId == -1)
		picture = QPixmap(":/MoodBox/Resources/avatar_all_friends.png");	
	else if(!pixmap.isNull())
		picture = pixmap;
	else
		INFOMANAGER->getContactPicture(contactId, picture, thumbnail);

	return picture;
}

void ContactAvatarButton::onContactPictureChanged(qint32 id)
{
	if (contactId == id)
		onPictureChanged();
}

}