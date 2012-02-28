#ifndef NEWVERSIONAVAILABLEDIALOG_H
#define NEWVERSIONAVAILABLEDIALOG_H

#include "uitools.h"

#include "ui_newversionavailabledialog.h"

namespace MoodBox
{

using namespace Ui;

class NewVersionAvailableDialog : public MoodBoxDialog, public NewVersionAvailableDialogClass
{
	Q_OBJECT

public:
	NewVersionAvailableDialog(QWidget *parent = NULL);

	bool getIsForcedParentWindowShow();

	void showDialog(QString descriptionText, bool isInteractive, bool isOverMainWindow);

private slots:
	void onSkip();

private:
	bool isForcedParentWindowShow;
};

}

#endif // NEWVERSIONAVAILABLEDIALOG_H
