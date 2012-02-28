#ifndef NEWCONTACTLISTWINDOW_H
#define NEWCONTACTLISTWINDOW_H

#include <QWidget>

#include "ui_newcontactlistwindow.h"
#include "userstatus.h"
#include "uitools.h"
#include "contactlistmenu.h"
#include "statusbutton.h"

class QMenu;
class QMouseEvent;

namespace MoodBox
{

class SetupDialog;
class FindDialog;
class AutoUpdater;

using namespace Ui;

class NewContactListWindow : public QWidget, public NewContactListWindowClass
{
	Q_OBJECT

public:
	NewContactListWindow(QWidget *parent = NULL);

	SetupDialog *setupDialog;

	// Moves window to the right of specified widget
	void updatePosition(const QWidget *topWidget);

	void initAutoUpdater(AutoUpdater *autoUpdater);

	void setStatusButtonIcon(const QIcon &icon) { statusButton->setIconLabel(icon.pixmap(QSize(1000, 1000))); }

	void onMessageReceived(qint32 userId, bool isAllFriends = false);

	void clearContacts() { if (newContactListWidget) newContactListWidget->clearContacts(); }

	int getUnreadMessagesForContact(qint32 id) const;
	int getUnreadMessagesForAllFriends() const;

signals:
	void contactSelected(qint32 id);
	void contactImageDrop(qint32 id, const QImage &image);

	void unreadContacts(int);

	void goOnline();
	void goOffline();
	void goLogout();
	void goExit();

	void soundStateChanged(const bool enableSounds);

	void historyCleared();

public slots:
	void onShowProfileDialog();
	void onShowFindDialog();
	void onShowNetworkSettingsDialog();
	void onUserStatusChanged(UserStatus::UserStatusEnum userStatus);
	void onResizeNeeded(int newX);

protected:
	virtual void closeEvent(QCloseEvent *event);
	virtual void resizeEvent(QResizeEvent *event);

private:
	FindDialog *findDialog;

	StatusButton *statusButton;
	ContactListMenu *menu;

private slots:
	// Buttons
	void on_setupToolButton_toggled(bool checked);
	void on_findButton_toggled(bool checked);
	void onStatusButtonClicked();
};

}

#endif // NEWCONTACTLISTWINDOW_H
