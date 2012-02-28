#include "setupdialog.h"

#include "formblocker.h"
#include "testtools.h"	

namespace MoodBox
{

SetupDialog::SetupDialog(QWidget *parent)
: MoodBoxDialog(parent), profileWasUpdated(false), settingsWereUpdated(false), previousTabIndex(0)
{
	TimeMeasure t("SetupDialog");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	UiTools::moveWindowToScreenCenter(this);

	setupTabWidget->setCurrentIndex((int) Profile);

	// Blocker
	formBlocker = new FormBlocker(this);
	formBlocker->addWidget(okButton);
	formBlocker->addWidget(cancelButton);
	
	formBlocker->addWidgets(profileFrame->getBlockingWidgets());
	formBlocker->addWidgets(settingsFrame->getBlockingWidgets());

	// Signals
	connect(okButton, SIGNAL(clicked()), this, SLOT(onStartUpdate()));
	connect(cancelButton, SIGNAL(clicked()), this, SLOT(onCancelUpdate()));
	connect(formBlocker, SIGNAL(progressCancelled()), this, SLOT(onProgressCancelled()));
	connect(closeToolButton, SIGNAL(clicked()), this, SLOT(reject()));

	connect(profileFrame, SIGNAL(updateFinished()), this, SLOT(onProfileUpdated()));
	connect(profileFrame, SIGNAL(updateError()), this, SLOT(onProgressCancelled()));
	connect(settingsFrame, SIGNAL(updateFinished()), this, SLOT(onSettingsUpdated()));

	connect(settingsFrame, SIGNAL(historyCleared()), this, SIGNAL(historyCleared()));
}

void SetupDialog::setCurrentTab(CurrentTabEnum tab)
{
	setupTabWidget->setCurrentIndex((int) tab);
	previousTabIndex = (int) tab;
}

void SetupDialog::initAutoUpdater(AutoUpdater *autoUpdater)
{
	settingsFrame->initAutoUpdater(autoUpdater);
}

void SetupDialog::showEvent(QShowEvent *event)
{
	MoodBoxDialog::showEvent(event);
	profileWasUpdated = false;
	settingsWereUpdated = false;

	// Checking accessibility of profile tab
	bool profileAccessbile = profileFrame->isAccessible();
	if (!profileAccessbile)
	{
		setCurrentTab(Settings);
	}
	
	setupTabWidget->setTabEnabled((int) Profile, profileAccessbile);
}

void SetupDialog::updateControls()
{
	profileFrame->updateControls();
	settingsFrame->init();
}

void SetupDialog::finishUpdate()
{
	formBlocker->unblock();
	emit soundStateChanged(settingsFrame->playSoundCheckBox->isChecked());
	accept();
}

void SetupDialog::on_setupTabWidget_currentChanged(int tabIndex)
{
	// Profile tab
	if (previousTabIndex == 0 && tabIndex != 0)
	{
		if (profileFrame->isValid())
		{	
			if (profileFrame->isChanged())
			{
				setupTabWidget->setCurrentIndex(0);
				if (UiTools::showDialog(this, tr(PROFILE_SAVE_TITLE), tr(PROFILE_SAVE_DESCRIPTION), QMessageBox::Yes | QMessageBox::No) == QMessageBox::Yes)
				{
					profileFrame->startUpdate();
					setupTabWidget->setCurrentIndex(tabIndex);
				}
				else
				{
					return;
				}
			}
		}
		else
		{
			setupTabWidget->setCurrentIndex(0);
			return;
		}
	}

	// Settings tab
	if (previousTabIndex == 1 && tabIndex != 1)
	{
		if (!settingsFrame->isValid())
		{
			// Go back if bad data
			setupTabWidget->setCurrentIndex(1);
			return;
		}
	}

	previousTabIndex = tabIndex;
}

void SetupDialog::onStartUpdate()
{
	if (!profileFrame->isValid() || !settingsFrame->isValid())
		return;

	formBlocker->block(true);
	
	profileFrame->startUpdate();
	settingsFrame->startUpdate();
}

void SetupDialog::onProgressCancelled()
{
	formBlocker->unblock();

	profileFrame->onRequestCancelled();
}

void SetupDialog::onCancelUpdate()
{
	onProgressCancelled();

	reject();
}

void SetupDialog::onProfileUpdated()
{
	profileWasUpdated = true;

	// Accept if other tabs were accepted
	if (settingsWereUpdated)
		finishUpdate();
}

void SetupDialog::onSettingsUpdated()
{
	settingsWereUpdated = true;

	// Accept if other tabs were accepted
	if (profileWasUpdated)
		finishUpdate();
}

}
