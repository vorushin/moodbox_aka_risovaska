#include "contactlistbutton.h"

#include <QPainter>

#include "debug.h"

namespace MoodBox
{

ContactListButton::ContactListButton(QWidget *parent)
	: QToolButton(parent),  mouseIsHovered(false), isPressed(false), contactsWithNewMessages(0)
{
	setIconSize(QSize(39, 39));

	updateImage();

	connect(this, SIGNAL(toggled(bool)), this, SLOT(updateImage()));
	connect(this, SIGNAL(pressed()), this, SLOT(onPressed()));
	connect(this, SIGNAL(released()), this, SLOT(onReleased()));
}

void ContactListButton::update(int contactsCountWithNewMessages)
{
	this->contactsWithNewMessages = contactsCountWithNewMessages;

	updateImage();
}

void ContactListButton::dragEnterEvent(QDragEnterEvent *event)
{
	if (!isChecked())
		setChecked(true);

	QToolButton::dragEnterEvent(event);
}

void ContactListButton::enterEvent(QEvent *event)
{
	QToolButton::enterEvent(event);
	mouseIsHovered = true;

	updateImage();
}

void ContactListButton::leaveEvent(QEvent *event)
{
	QToolButton::leaveEvent(event);
	mouseIsHovered = false;

	updateImage();
}

void ContactListButton::updateImage()
{
	QPixmap tvMask(":/MoodBox/Resources/tv_icon_mask.png");
	static QIcon tvImage(":/MoodBox/Resources/tv_icon.png");
	static QIcon tvImageHovered(":/MoodBox/Resources/tv_icon_hover.png");
	static QIcon tvImagePressed(":/MoodBox/Resources/tv_icon_pressed.png");
	static QIcon circle(":/MoodBox/Resources/tv_circle.png");
	static QIcon circleHovered(":/MoodBox/Resources/tv_circle_hover.png");
	static QIcon circlePressed(":/MoodBox/Resources/tv_circle_pressed.png");

	QPainter imgPainter(&tvMask);
	imgPainter.setCompositionMode(QPainter::CompositionMode_SourceOver);

	if (contactsWithNewMessages > 0)
	{
		QRect position;

		if (isChecked() || isPressed)
		{
			circlePressed.paint(&imgPainter, tvMask.rect(), Qt::AlignCenter, QIcon::Normal);
			position = QRect(24, 23, 13, 13);
		}
		else
		{
			if (mouseIsHovered)
				circleHovered.paint(&imgPainter, tvMask.rect(), Qt::AlignCenter, QIcon::Normal);
			else
				circle.paint(&imgPainter, tvMask.rect(), Qt::AlignCenter, QIcon::Normal);

			position = QRect(24, 23, 13, 13);
		}

		imgPainter.setPen(Qt::white);
		int fontSize = 0;
		
#ifdef WIN32
		fontSize = 7;
#else
		fontSize = 9;
#endif
		
		imgPainter.setFont(QFont("Verdana", fontSize, QFont::Bold));

		QString unreadContacts;
		if (contactsWithNewMessages > 9)
			unreadContacts = "!";
		else
			unreadContacts = QString("%1").arg(contactsWithNewMessages);

		QRect boundingRect = imgPainter.boundingRect(position, Qt::AlignCenter, unreadContacts);
		imgPainter.drawText(boundingRect, Qt::AlignCenter, unreadContacts);

		//imgPainter.drawRect(QRect(position));
	}
	else
	{
		if (isChecked() || isPressed)
			tvImagePressed.paint(&imgPainter, tvMask.rect(), Qt::AlignCenter, QIcon::Normal);
		else
			if (mouseIsHovered)
				tvImageHovered.paint(&imgPainter, tvMask.rect(), Qt::AlignCenter, QIcon::Normal);
			else
				tvImage.paint(&imgPainter, tvMask.rect(), Qt::AlignCenter, QIcon::Normal);
	}

	setIcon(tvMask);
}

void ContactListButton::onPressed()
{
	isPressed = true;

	updateImage();
}

void ContactListButton::onReleased()
{
	isPressed = false;

	updateImage();
}

}
