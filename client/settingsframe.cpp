#include "settingsframe.h"

#include <QValidator>
#include <QMessageBox>

#include "apptools.h"
#include "autoupdater.h"
#include "programsettings.h"
#include "testtools.h"
#include "peopleinfomanager.h"
#include "messageorganizer.h"

namespace MoodBox
{

SettingsFrame::SettingsFrame(QWidget *parent)
	: SetupDialogFrame(parent), autoUpdater(NULL)
{
	TimeMeasure t("SettingsFrame");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	//serverPortCombo->addItem(tr(DEFAULT_SERVER_PORT_TEXT).arg(DEFAULT_SERVER_PORT), DEFAULT_SERVER_PORT);
	//serverPortCombo->addItem(tr(ALTERNATE_SERVER_PORT_TEXT).arg(ALTERNATE_SERVER_PORT), ALTERNATE_SERVER_PORT);

	proxyTypeCombo->addItem(tr(PROXY_TYPE_SOCKS), QNetworkProxy::Socks5Proxy);
	proxyTypeCombo->addItem(tr(PROXY_TYPE_HTTP), QNetworkProxy::HttpProxy);

	// Create form verifiers
	proxyVerifier = new FormVerifier(this);
	proxyAuthenticationVerifier = new FormVerifier(this);

	// Proxy host verifier
	QValidator *hostValidator = new QRegExpValidator(QRegExp("\\S+"), this);
	proxyVerifier->addVerifier(new LineEditVerifier(true, this, proxyHostLabel, hostValidator, tr(INVALID_PROXY_HOST)));

	// Proxy port verifier
	QValidator *portValidator = new QIntValidator(1, 65535, this);
	proxyVerifier->addVerifier(new LineEditVerifier(true, this, proxyPortLabel, portValidator, tr(INVALID_PROXY_PORT)));

	// Proxy login verifier
	QValidator *loginValidator = new QRegExpValidator(QRegExp("\\S.*"), this);
	proxyAuthenticationVerifier->addVerifier(new LineEditVerifier(true, this, proxyLoginLabel, loginValidator, tr(INVALID_PROXY_USER_NAME)));

	// Hide server connection port option
	serverConnectionPortLabel->hide();
	serverPortCombo->hide();
}

void SettingsFrame::initAutoUpdater(AutoUpdater *autoUpdater)
{
	this->autoUpdater = autoUpdater;

	autoUpdateCheckBox->setChecked(autoUpdater->isAutoUpdateEnabled());
}

void SettingsFrame::init()
{
	readSettings();

	updateProxySettingsEnabledState();
	updateProxyPort();
}

bool SettingsFrame::isValid()
{
	bool isValid = true;

	// Hide previous errors, if any
	proxyVerifier->highlight(WidgetVerifier::Normal);
	proxyAuthenticationVerifier->highlight(WidgetVerifier::Normal);

	if (useProxyCheckBox->isChecked())
	{
		isValid = proxyVerifier->verifyAndHighlight(false);

		if (proxyRequiresAuthenticationCheckBox->isChecked())
			isValid = isValid && proxyAuthenticationVerifier->verifyAndHighlight(false);
	}

	return isValid;
}

void SettingsFrame::startUpdate()
{
	if (!isValid())
		return;

	autoUpdater->setAutoUpdateEnabled(autoUpdateCheckBox->isChecked());

	ProgramSettings settings;

	settings.autoLaunchProgram = launchOnStartupCheckBox->isChecked();
	
	settings.playSounds = playSoundCheckBox->isChecked();

	settings.serverPort = serverPortCombo->itemData(serverPortCombo->currentIndex()).toInt();

	settings.useProxy = useProxyCheckBox->isChecked();
	settings.proxyType = getSelectedProxyType();
	settings.proxyHost = proxyHostEdit->text().trimmed();
	settings.proxyPortSocks = socksProxyPort;
	settings.proxyPortHttp = httpProxyPort;
	settings.proxyRequiresAuthorization = proxyRequiresAuthenticationCheckBox->isChecked();
	settings.proxyUserName = proxyLoginEdit->text().trimmed();
	settings.proxyPassword = proxyPasswordEdit->text();

	settings.writeSettings();
	settings.apply();

	emit updateFinished();
}

void SettingsFrame::readSettings()
{
	ProgramSettings settings;
	settings.readSettings();

	launchOnStartupCheckBox->setChecked(settings.autoLaunchProgram);

	playSoundCheckBox->setChecked(settings.playSounds);

	serverPortCombo->setCurrentIndex(serverPortCombo->findData(settings.serverPort));

	useProxyCheckBox->setChecked(settings.useProxy);
	setProxyType(settings.proxyType);
	proxyHostEdit->setText(settings.proxyHost);
	socksProxyPort = settings.proxyPortSocks;
	httpProxyPort = settings.proxyPortHttp;

	proxyRequiresAuthenticationCheckBox->setChecked(settings.proxyRequiresAuthorization);
	proxyLoginEdit->setText(settings.proxyUserName);
	proxyPasswordEdit->setText(settings.proxyPassword);
}

void SettingsFrame::updateProxyPort()
{
	QString value;

	switch (getSelectedProxyType())
	{
		case QNetworkProxy::Socks5Proxy:
			value = socksProxyPort;
			break;
		case QNetworkProxy::HttpProxy:
			value = httpProxyPort;
			break;
	}

	proxyPortEdit->setText(value);
}

void SettingsFrame::setProxyPort(QString value, bool display)
{
	switch (getSelectedProxyType())
	{
		case QNetworkProxy::Socks5Proxy:
			socksProxyPort = value;
			break;

		case QNetworkProxy::HttpProxy:
			httpProxyPort = value;
			break;
	}

	if (display)
		proxyPortEdit->setText(value);
}

void SettingsFrame::updateProxySettingsEnabledState()
{
	bool isEnabled = useProxyCheckBox->isChecked();

	proxyTypeLabel->setEnabled(isEnabled);
	proxyTypeCombo->setEnabled(isEnabled);
	proxyHostLabel->setEnabled(isEnabled);
	proxyHostEdit->setEnabled(isEnabled);
	proxyPortLabel->setEnabled(isEnabled);
	proxyPortEdit->setEnabled(isEnabled);
	setStandardProxyPortButton->setEnabled(isEnabled);
	proxyRequiresAuthenticationCheckBox->setEnabled(isEnabled);

	updateProxyAuthenticationSettingsEnabledState();
}

void SettingsFrame::updateProxyAuthenticationSettingsEnabledState()
{
	bool isEnabled = useProxyCheckBox->isChecked() && proxyRequiresAuthenticationCheckBox->isChecked();

	proxyLoginLabel->setEnabled(isEnabled);
	proxyLoginEdit->setEnabled(isEnabled);
	proxyPasswordLabel->setEnabled(isEnabled);
	proxyPasswordEdit->setEnabled(isEnabled);
}

QNetworkProxy::ProxyType SettingsFrame::getSelectedProxyType()
{
	return (QNetworkProxy::ProxyType)proxyTypeCombo->itemData(proxyTypeCombo->currentIndex()).toInt();
}

void SettingsFrame::setProxyType(QNetworkProxy::ProxyType type)
{
	proxyTypeCombo->setCurrentIndex(proxyTypeCombo->findData(type));
}

void SettingsFrame::on_checkForNewVersionButton_clicked()
{
	autoUpdater->checkForUpdateInteractive(this);
}

void SettingsFrame::on_useProxyCheckBox_stateChanged(int newState)
{
	Q_UNUSED(newState)

	updateProxySettingsEnabledState();
}

void SettingsFrame::on_proxyRequiresAuthenticationCheckBox_stateChanged(int newState)
{
	Q_UNUSED(newState)

	updateProxyAuthenticationSettingsEnabledState();
}

void SettingsFrame::on_proxyTypeCombo_currentIndexChanged(int index)
{
	Q_UNUSED(index)

	updateProxyPort();
}

void SettingsFrame::on_proxyPortEdit_textEdited(const QString &text)
{
	setProxyPort(text, false);
}

void SettingsFrame::on_setStandardProxyPortButton_clicked()
{
	switch (getSelectedProxyType())
	{
		case QNetworkProxy::Socks5Proxy:
			setProxyPort(ProgramSettings::getDefaultSocksProxyPort());
			break;

		case QNetworkProxy::HttpProxy:
			setProxyPort(ProgramSettings::getDefaultHttpProxyPort());
			break;
	}
}

void SettingsFrame::on_clearHistoryButton_clicked()
{
	if (UiTools::showDialog(this, tr(CLEAR_HISTORY_DIALOG_TITLE), tr(CLEAR_HISTORY_DIALOG_DESCRIPTION), QMessageBox::Yes | QMessageBox::No) == QMessageBox::Yes)
	{
		QString dataDir = MessageOrganizer::getHistoryFolder();
		
		if (UiTools::rmDirRecursive(dataDir))
			UiTools::showDialog(this, tr(HISTORY_DATA_REMOVAL_TITLE), tr(HISTORY_DATA_REMOVED_DESCRIPTION), QMessageBox::Ok);
		else
			UiTools::showDialog(this, tr(HISTORY_DATA_REMOVAL_TITLE), tr(HISTORY_DATA_NOT_REMOVED_DESCRIPTION), QMessageBox::Ok);

		emit historyCleared();
	}
}

}



