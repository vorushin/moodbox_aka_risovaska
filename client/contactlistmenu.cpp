#include "contactlistmenu.h"

#include "uitools.h"
#include "peopleinfomanager.h"

namespace MoodBox
{

ContactListMenu::ContactListMenu(QWidget *parent)
	: QWidget(parent, Qt::Popup)
{
	setupUi(this);

	connect(onlineButton, SIGNAL(clicked()), this, SIGNAL(goOnline()));
	connect(offlineButton, SIGNAL(clicked()), this, SIGNAL(goOffline()));
	connect(logoutButton, SIGNAL(clicked()), this, SIGNAL(goLogout()));
	connect(exitButton, SIGNAL(clicked()), this, SIGNAL(goExit()));

	connect(onlineButton, SIGNAL(released()), this, SLOT(close()));
	connect(offlineButton, SIGNAL(released()), this, SLOT(close()));
	connect(logoutButton, SIGNAL(released()), this, SLOT(close()));
	connect(exitButton, SIGNAL(released()), this, SLOT(close()));
}

void ContactListMenu::showEvent(QShowEvent *event)
{
	onlineButton->setEnabled(true);
	offlineButton->setEnabled(true);

	if (INFOMANAGER->getIsLoggedOn())
	{
		UserStatus::UserStatusEnum status = INFOMANAGER->getUserStatus();

		if (status == UserStatus::Online)
			onlineButton->setEnabled(false);
		else
			offlineButton->setEnabled(false);
	}

	emit menuAboutToShow();

	QWidget::showEvent(event);
}

void ContactListMenu::closeEvent(QCloseEvent *event)
{
	emit menuAboutToHide();

	QWidget::closeEvent(event);
}

void ContactListMenu::mouseReleaseEvent(QMouseEvent *event)
{
	close();

	QWidget::mouseReleaseEvent(event);
}

void ContactListMenu::resizeEvent(QResizeEvent *event)
{
	setMask(WidgetMaskCreator::createMask(WidgetMaskCreator::ContactListMenu, width(), height()));
	
	QWidget::resizeEvent(event);
}

}