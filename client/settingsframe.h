#ifndef SETTINGSFRAME_H
#define SETTINGSFRAME_H

#include "setupdialogframe.h"
#include <QNetworkProxy>

#include "ui_settingsframe.h"
#include "verifiers.h"

namespace MoodBox
{

using namespace Ui;

#define PROXY_TYPE_SOCKS					"SOCKS 5"
#define PROXY_TYPE_HTTP						"HTTP"

#define INVALID_PROXY_HOST					QT_TRANSLATE_NOOP("MoodBox::SettingsFrame", "InvalidProxyHost")
#define INVALID_PROXY_PORT					QT_TRANSLATE_NOOP("MoodBox::SettingsFrame", "InvalidProxyPort")
#define INVALID_PROXY_USER_NAME				QT_TRANSLATE_NOOP("MoodBox::SettingsFrame", "InvalidProxyLogin")

#define CLEAR_HISTORY_DIALOG_TITLE			QT_TRANSLATE_NOOP("MoodBox::SettingsFrame", "ClearHistoryDialogTitle")
#define CLEAR_HISTORY_DIALOG_DESCRIPTION	QT_TRANSLATE_NOOP("MoodBox::SettingsFrame", "ClearHistoryDialogDescription")

#define HISTORY_DATA_REMOVAL_TITLE			QT_TRANSLATE_NOOP("MoodBox::SettingsFrame", "HistoryDataRemovalTitle")
#define HISTORY_DATA_REMOVED_DESCRIPTION	QT_TRANSLATE_NOOP("MoodBox::SettingsFrame", "HistoryDataRemovedDescription")
#define HISTORY_DATA_NOT_REMOVED_DESCRIPTION	QT_TRANSLATE_NOOP("MoodBox::SettingsFrame", "HistoryDataNotRemovedDescription")

class AutoUpdater;

// Frame for settings (network settings, auto update, auto launch)
class SettingsFrame : public SetupDialogFrame, public SettingsFrameClass
{
	Q_OBJECT

public:
	SettingsFrame(QWidget *parent);
	
	void initAutoUpdater(AutoUpdater *autoUpdater);

	virtual bool isValid();
	virtual void startUpdate();

	void init();

signals:
	void historyCleared();

private:
	QString socksProxyPort;
	QString httpProxyPort;
	
	FormVerifier *proxyVerifier;
	FormVerifier *proxyAuthenticationVerifier;

	AutoUpdater *autoUpdater;

	void readSettings();

	void updateProxyPort();
	void setProxyPort(QString value, bool display = true);

	void updateProxySettingsEnabledState();
	void updateProxyAuthenticationSettingsEnabledState();

	QNetworkProxy::ProxyType getSelectedProxyType();
	void setProxyType(QNetworkProxy::ProxyType type);

private slots:
	void on_checkForNewVersionButton_clicked();
	void on_useProxyCheckBox_stateChanged(int newState);
	void on_proxyRequiresAuthenticationCheckBox_stateChanged(int newState);
	void on_proxyTypeCombo_currentIndexChanged(int index);
	void on_proxyPortEdit_textEdited(const QString &text);
	void on_setStandardProxyPortButton_clicked();
	void on_clearHistoryButton_clicked();
};

}

#endif // SETTINGSFRAME_H
