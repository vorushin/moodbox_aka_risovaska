#ifndef SETUPDIALOG_H
#define SETUPDIALOG_H

#include "uitools.h"

#include "ui_setupdialog.h"

namespace MoodBox
{

using namespace Ui;

class FormBlocker;
class AutoUpdater;

#define PROFILE_SAVE_TITLE			QT_TRANSLATE_NOOP("MoodBox::SetupDialog", "SaveProfileDialogTitle")	
#define PROFILE_SAVE_DESCRIPTION	QT_TRANSLATE_NOOP("MoodBox::SetupDialog", "SaveProfileDialogText")	

// Setup dialog (for: profile, settings, blocked list)
class SetupDialog : public MoodBoxDialog, public SetupDialogClass
{
	Q_OBJECT

public:
	enum CurrentTabEnum { Profile = 0, Settings = 1 };

	SetupDialog(QWidget *parent);

	void setCurrentTab(CurrentTabEnum tab);

	void initAutoUpdater(AutoUpdater *autoUpdater);

	void updateControls();

signals:
	void soundStateChanged(const bool enableSounds);
	void historyCleared();

protected:
	virtual void showEvent(QShowEvent *event);

private:
	bool profileWasUpdated;
	bool settingsWereUpdated;

	FormBlocker *formBlocker;

	int previousTabIndex;

	void finishUpdate();

private slots:
	void on_setupTabWidget_currentChanged(int tabIndex);

	void onStartUpdate();
	void onProgressCancelled();
	void onCancelUpdate();

	void onProfileUpdated();
	void onSettingsUpdated();
};

}

#endif // SETUPDIALOG_H
