#include "useravatarbutton.h"

#include "peopleinfomanager.h"
#include "picturefileloader.h"

namespace MoodBox
{

UserAvatarButton::UserAvatarButton(QWidget *parent)
	: AvatarButton(parent), newAvatarSet(false)
{
	connect(INFOMANAGER, SIGNAL(userPictureChanged()), this, SLOT(onPictureChanged()));
	connect(PICTURELOADER, SIGNAL(newPictureLoaded(const QString &)), this, SLOT(onNewPictureLoaded(const QString &)));
}

void UserAvatarButton::setNewAvatar(const QPixmap &avatar)
{
	this->newAvatar = avatar;
	newAvatarSet = true;

	updateCache();
}

void UserAvatarButton::showEvent(QShowEvent *event)
{
	updateCache();

	QWidget::showEvent(event);
}

QPixmap UserAvatarButton::getSourcePicture(bool thumbnail)
{
	static QPixmap defaultUserPic(USERPIC_DEFAULT);

	// Check the new avatar, if it is bad let's use default
	if (newAvatarSet)
		return (newAvatar.isNull()) ? defaultUserPic : newAvatar;

	// If not logged in let's use default too
	if (!INFOMANAGER->getIsLoggedOn())
		return defaultUserPic;

	// Update the cache if needed
	QPixmap picture;
	QString id = QString::number(INFOMANAGER->getUserAccount().getId());
	bool find = PICTURELOADER->find(id, picture, INFOMANAGER->getUserAccount().getUserpicUrl());

	if (!find)
		picture = defaultUserPic;

	// Resize the result
	PeopleInfoManager::resizePicture(picture, thumbnail);

	return picture;
}

void UserAvatarButton::onNewPictureLoaded(const QString &key)
{
	if (!INFOMANAGER->getIsLoggedOn() || key.toInt() != INFOMANAGER->getUserAccount().getId())
		return;

	onPictureChanged();
}

}
