#ifndef USERAVATARBUTTON_H
#define USERAVATARBUTTON_H

#include <QWidget>

#include "avatarbutton.h"

namespace MoodBox
{
// Class for manage user avatar
class UserAvatarButton : public AvatarButton
{
	Q_OBJECT

public:
	UserAvatarButton(QWidget *parent);

	void setNewAvatar(const QPixmap &avatar);
	inline QPixmap getNewAvatar() const { return newAvatar; };

protected:
	virtual QPixmap getSourcePicture(bool thumbnail);

	virtual void showEvent(QShowEvent *event);

private:
	bool newAvatarSet;

	QPixmap newAvatar;

private slots:
	void onNewPictureLoaded(const QString &key);
};

}

#endif // USERAVATARBUTTON_H
