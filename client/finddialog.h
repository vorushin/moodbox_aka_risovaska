#ifndef FINDDIALOG_H
#define FINDDIALOG_H

#include "servercontrol.h"

#include "ui_finddialog.h"

namespace MoodBox
{

using namespace Ui;

// Find people and channels dialog
class FindDialog : public ServerDialog, public FindDialogClass
{
	Q_OBJECT

public:
	FindDialog(QWidget *parent = 0);
	
	void clearData();

protected:
	virtual void resizeEvent(QResizeEvent *event);
	virtual void showEvent(QShowEvent *event);

private slots:
	void on_addUserButton_clicked();
	void on_findTabWidget_currentChanged(int tabIndex);

	void onClose();
};

}

#endif // FINDDIALOG_H
