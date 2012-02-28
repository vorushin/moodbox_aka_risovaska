#ifndef CONTACTAVATARBUTTON_H
#define CONTACTAVATARBUTTON_H

#include <QWidget>

#include "avatarbutton.h"

namespace MoodBox
{
// Class for manage contact avatar
class ContactAvatarButton : public AvatarButton
{
	Q_OBJECT

public:
	ContactAvatarButton(QWidget *parent);

	void setContactId(qint32 contactId);

	// use it if user is not in contacts
	void setPicture(const QPixmap &picture);

protected:
	virtual QPixmap getSourcePicture(bool thumbnail);

protected slots:
	void onContactPictureChanged(qint32 id);
	
private:
	qint32 contactId;
	QPixmap pixmap;
};

}

#endif // CONTACTAVATARBUTTON_H
