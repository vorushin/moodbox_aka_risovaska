#include "newcontactlistwindow.h"

#include <QBitmap>
#include <QMouseEvent>
#include <QPainter>
#include <QPixmap>
#include <QDesktopWidget>
#include <QSettings>

#include "setupdialog.h"
#include "finddialog.h"
#include "peopleinfomanager.h"
#include "international.h"
#include "common.h"
#include "testtools.h"
#include "statusicon.h"
#include "debug.h"
#include "language.h"

#ifdef Q_WS_MAC
#include "mactools.h"
#endif
#ifdef Q_WS_X11
#include "linuxtools.h"
#endif

namespace MoodBox
{

NewContactListWindow::NewContactListWindow(QWidget *parent)
:	QWidget(parent, Qt::FramelessWindowHint | Qt::Tool), 
	setupDialog(NULL), findDialog(NULL)
{
#ifdef Q_WS_MAC
    MacTools::addWindow(this);
#endif
#ifdef Q_WS_X11
    LinuxTools::addWindow(this);
#endif

	TimeMeasure t("NewContactListWindow");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	connect(INFOMANAGER, SIGNAL(userStatusChanged(UserStatus::UserStatusEnum)), this, SLOT(onUserStatusChanged(UserStatus::UserStatusEnum)));

	connect(newContactListWidget, SIGNAL(contactSelected(qint32)), this, SIGNAL(contactSelected(qint32)));
	connect(newContactListWidget, SIGNAL(contactImageDrop(qint32, const QImage &)), this, SIGNAL(contactImageDrop(qint32, const QImage &)));
	connect(newContactListWidget, SIGNAL(unreadContacts(int)), this, SIGNAL(unreadContacts(int)));

	menu = new ContactListMenu(this);
	connect(menu, SIGNAL(goOnline()), this, SIGNAL(goOnline()));
	connect(menu, SIGNAL(goOffline()), this, SIGNAL(goOffline()));
	connect(menu, SIGNAL(goLogout()), this, SIGNAL(goLogout()));
	connect(menu, SIGNAL(goExit()), this, SIGNAL(goExit()));

	statusButton = new StatusButton(this);
	verticalLayout->addWidget(statusButton);
	connect(statusButton, SIGNAL(clicked()), this, SLOT(onStatusButtonClicked()));
	connect(menu, SIGNAL(menuAboutToShow()), statusButton, SLOT(onMenuAboutToShow()));
	connect(menu, SIGNAL(menuAboutToHide()), statusButton, SLOT(onMenuAboutToHide()));

	onUserStatusChanged(INFOMANAGER->getUserStatus());

	setupDialog = new SetupDialog(this);
	findDialog = new FindDialog(this);

	connect(setupDialog, SIGNAL(finished(int)), setupToolButton, SLOT(toggle()));
	connect(setupDialog, SIGNAL(soundStateChanged(const bool)), this, SIGNAL(soundStateChanged(const bool)));
	connect(findDialog, SIGNAL(finished(int)), findButton, SLOT(toggle()));
	connect(gripWidget, SIGNAL(resizeNeeded(int)), this, SLOT(onResizeNeeded(int)));

	connect(setupDialog, SIGNAL(historyCleared()), this, SIGNAL(historyCleared()));
}

void NewContactListWindow::updatePosition(const QWidget *topWidget)
{
	setGeometry(topWidget->geometry().x() - width() + 12 /*TODO*/, 
		topWidget->geometry().y(), width(), topWidget->height());
}

void NewContactListWindow::initAutoUpdater(AutoUpdater *autoUpdater)
{
	setupDialog->initAutoUpdater(autoUpdater);
}

void NewContactListWindow::onMessageReceived(qint32 userId, bool isAllFriends)
{
	newContactListWidget->onMessageReceived(userId, isAllFriends);
}

int NewContactListWindow::getUnreadMessagesForContact(qint32 id) const
{
	return newContactListWidget->getUnreadMessagesForContact(id);
}

int NewContactListWindow::getUnreadMessagesForAllFriends() const
{
	return newContactListWidget->getUnreadMessagesForAllFriends();
}

void NewContactListWindow::onShowProfileDialog()
{
	setupToolButton->setChecked(true);

	setupDialog->setCurrentTab(SetupDialog::Profile);
	setupDialog->updateControls();
	setupDialog->show();
	setupDialog->raise();
}

void NewContactListWindow::onShowFindDialog()
{
	findButton->setChecked(true);

	findDialog->clearData();
	findDialog->show();
	findDialog->raise();
}

void NewContactListWindow::onShowNetworkSettingsDialog()
{
	setupToolButton->setChecked(true);

	setupDialog->setCurrentTab(SetupDialog::Settings);
	setupDialog->show();
	setupDialog->raise();
}

void NewContactListWindow::onUserStatusChanged(UserStatus::UserStatusEnum userStatus)
{
	switch (userStatus)
	{
		case UserStatus::Online:
			statusButton->setIconLabel(QPixmap(":/MoodBox/Resources/tv_green_icon.png"));
#ifdef RUSSIAN_VERSION
			statusButton->setTextLabel(QPixmap(":/MoodBox/Resources/cl_menu_online_ru.png"));
#else
			statusButton->setTextLabel(QPixmap(":/MoodBox/Resources/cl_menu_online.png"));
#endif
			statusButton->setArrowLabel(QPixmap(":/MoodBox/Resources/cl_right_arrow_green.png"), QPixmap(":/MoodBox/Resources/cl_down_arrow_green.png"));
			
			newContactListWidget->onOnline();
			break;

		case UserStatus::Offline:
			statusButton->setIconLabel(QPixmap(":/MoodBox/Resources/tv_red_icon.png"));
#ifdef RUSSIAN_VERSION
			statusButton->setTextLabel(QPixmap(":/MoodBox/Resources/cl_menu_offline_ru.png"));
#else
			statusButton->setTextLabel(QPixmap(":/MoodBox/Resources/cl_menu_offline.png"));
#endif
			statusButton->setArrowLabel(QPixmap(":/MoodBox/Resources/cl_right_arrow_red.png"), QPixmap(":/MoodBox/Resources/cl_down_arrow_red.png"));

			newContactListWidget->onOffline();
			break;

		case UserStatus::Connecting:
			statusButton->setIconLabel(QPixmap(":/MoodBox/Resources/tv_red_icon.png"));
#ifdef RUSSIAN_VERSION
			statusButton->setTextLabel(QPixmap(":/MoodBox/Resources/cl_menu_connecting_ru.png"));
#else
			statusButton->setTextLabel(QPixmap(":/MoodBox/Resources/cl_menu_connecting.png"));
#endif
			statusButton->setArrowLabel(QPixmap(":/MoodBox/Resources/cl_right_arrow_green.png"), QPixmap(":/MoodBox/Resources/cl_down_arrow_green.png"));
			break;
	}
}

void NewContactListWindow::closeEvent(QCloseEvent *event)
{
	hide();

	event->ignore();
}

void NewContactListWindow::resizeEvent(QResizeEvent *event)
{
	setMask(WidgetMaskCreator::createMask(WidgetMaskCreator::WidgetMaskContacts, width(), height()));

	QWidget::resizeEvent(event);
}

void NewContactListWindow::on_setupToolButton_toggled(bool checked)
{
	(checked) ? onShowProfileDialog() : setupDialog->hide();
}

void NewContactListWindow::on_findButton_toggled(bool checked)
{
	if (checked)
	{
		findDialog->clearData();
		findDialog->show();
	}
	else
		findDialog->hide();
}

void NewContactListWindow::onStatusButtonClicked()
{
	QRect availableGeometry = QApplication::desktop()->availableGeometry(this);
	QPoint p = statusButton->mapToGlobal(QPoint(0, menu->height() + statusButton->height()));
	int y = (p.y() > availableGeometry.bottom()) ? -menu->height() : statusButton->height();
	menu->move(statusButton->mapToGlobal(QPoint(0, y)));
	menu->setFixedWidth(statusButton->width());

	menu->show();
}

void NewContactListWindow::onResizeNeeded(int newX)
{
	if (newX > geometry().right() - minimumSize().width())
		newX = geometry().right() - minimumSize().width();

	setGeometry(newX, geometry().y(), geometry().width() - (newX - geometry().x()), geometry().height());
}

}
