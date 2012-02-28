#ifndef CONTACTLISTMENU_H
#define CONTACTLISTMENU_H

#include <QWidget>

#include "ui_contactlistmenu.h"

namespace MoodBox
{

using namespace Ui;

class ContactListMenu : public QWidget, public ContactListMenuClass
{
	Q_OBJECT

public:
	ContactListMenu(QWidget *parent = NULL);

signals:
	void menuAboutToShow();
	void menuAboutToHide();

	// Menu
	void goOnline();
	void goOffline();
	void goLogout();
	void goExit();

protected:
	virtual void showEvent(QShowEvent *event);
	virtual void closeEvent(QCloseEvent *event);
	virtual void mouseReleaseEvent(QMouseEvent *event);
	virtual void resizeEvent(QResizeEvent *event);
};

}

#endif // CONTACTLISTMENU_H
