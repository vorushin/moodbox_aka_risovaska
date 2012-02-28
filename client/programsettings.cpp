#include "programsettings.h"

#include <QSettings>
#include <QDir>

#include "apptools.h"
#include "common.h"

#ifdef Q_WS_MAC
#include "mactools.h"
#endif

namespace MoodBox
{

ProgramSettings::ProgramSettings()
{
	autoLaunchProgram = false;

	serverPort = DEFAULT_SERVER_PORT;

	useProxy = false;
	proxyType = QNetworkProxy::NoProxy;
	proxyRequiresAuthorization = false;
}

int ProgramSettings::getProxyPort() const
{
	QString portStr;

	switch (proxyType)
	{
		case QNetworkProxy::Socks5Proxy:
			portStr = proxyPortSocks;
			break;

		case QNetworkProxy::HttpProxy:
			portStr = proxyPortHttp;
			break;
	}

	bool ok;
	int port = portStr.toInt(&ok);

	if (ok && port > 0 && port <=65535)
		return port;

	return -1;
}

void ProgramSettings::readSettings()
{
	autoLaunchProgram = getIsStartingWhenComputerStarts();

	QSettings settings;

	settings.beginGroup(APP_OPTION_GROUP);
	playSounds = settings.value(PLAY_SOUNDS_OPTION).toBool();
	settings.endGroup();

	settings.beginGroup(NETWORK_SETTINGS_GROUP);

	serverPort = settings.value(SERVER_PORT_OPTION, DEFAULT_SERVER_PORT).toInt();
	if (serverPort != DEFAULT_SERVER_PORT && serverPort != ALTERNATE_SERVER_PORT)
		serverPort = DEFAULT_SERVER_PORT;

	settings.beginGroup(PROXY_SETTINGS_SUBGROUP);

	useProxy = settings.value(USE_PROXY_OPTION, false).toBool();

	proxyType = (QNetworkProxy::ProxyType)settings.value(PROXY_TYPE_OPTION, QNetworkProxy::Socks5Proxy).toInt();
	
	proxyHost = settings.value(PROXY_HOST_OPTION, QString()).toString();
	proxyPortSocks = settings.value(PROXY_PORT_SOCKS_OPTION, getDefaultSocksProxyPort()).toString();
	proxyPortHttp = settings.value(PROXY_PORT_HTTP_OPTION, getDefaultHttpProxyPort()).toString();

	proxyRequiresAuthorization = settings.value(PROXY_REQUIRES_AUTHORIZATION_OPTION, false).toBool();

	proxyUserName = settings.value(PROXY_USER_NAME_OPTION, QString()).toString();
	proxyPassword = settings.value(PROXY_PASSWORD_OPTION, QString()).toString();

	settings.endGroup();
	settings.endGroup();

	// Reading settings from ini file
	QSettings serverSettings(AppTools::getPathRelativeToExeDir(SERVER_SETTINGS_FILE), QSettings::IniFormat);
	serverSettings.beginGroup(PROXY_HOST_OPTION);
	serverUrl = serverSettings.value(PROXY_HOST_OPTION, DEFAULT_SERVER_URL).toString();
	serverSettings.endGroup();
}

void ProgramSettings::writeSettings() const
{
	setIsStartingWhenComputerStarts(autoLaunchProgram);

	QSettings settings;

	settings.beginGroup(APP_OPTION_GROUP);
	settings.setValue(PLAY_SOUNDS_OPTION, playSounds);
	settings.endGroup();

	settings.beginGroup(NETWORK_SETTINGS_GROUP);
	settings.setValue(SERVER_PORT_OPTION, serverPort);

	settings.beginGroup(PROXY_SETTINGS_SUBGROUP);

	settings.setValue(USE_PROXY_OPTION, useProxy);
	settings.setValue(PROXY_TYPE_OPTION, proxyType);

	settings.setValue(PROXY_HOST_OPTION, proxyHost);
	settings.setValue(PROXY_PORT_SOCKS_OPTION, proxyPortSocks);
	settings.setValue(PROXY_PORT_HTTP_OPTION, proxyPortHttp);

	settings.setValue(PROXY_REQUIRES_AUTHORIZATION_OPTION, proxyRequiresAuthorization);
	settings.setValue(PROXY_USER_NAME_OPTION, proxyUserName);
	settings.setValue(PROXY_PASSWORD_OPTION, proxyPassword);

	settings.endGroup();
	settings.endGroup();
}

void ProgramSettings::apply()
{
	QNetworkProxy::setApplicationProxy(getProxySettings(*this));
}

void ProgramSettings::applyProxySettings()
{
	ProgramSettings programSettings;
	programSettings.readSettings();
	QNetworkProxy::setApplicationProxy(getProxySettings(programSettings));
}

QNetworkProxy ProgramSettings::getProxySettings(const ProgramSettings &programSettings)
{
	if (programSettings.useProxy)
	{
		QString userName;
		QString password;

		if (programSettings.proxyRequiresAuthorization)
		{
			userName = programSettings.proxyUserName;
			password = programSettings.proxyPassword;
		}

		return QNetworkProxy(programSettings.proxyType, programSettings.proxyHost, programSettings.getProxyPort(), userName, password);
	}
	else
		return QNetworkProxy(QNetworkProxy::DefaultProxy);
}

QString ProgramSettings::getDefaultSocksProxyPort()
{
	return QString::number(DEFAULT_SOCKS_PROXY_PORT);
}

QString ProgramSettings::getDefaultHttpProxyPort()
{
	return QString::number(DEFAULT_HTTP_PROXY_PORT);
}

#ifdef WIN32

bool  ProgramSettings::getIsStartingWhenComputerStarts()
{
	QSettings settings("Microsoft", "Windows");
	settings.beginGroup("CurrentVersion/Run");
	QString value = settings.value(APP_NAME, QString()).toString();

	bool keyExists = !value.isNull();

	if (keyExists)
	{
		bool keyMatches = (value.compare(getOnComputerStartStartString(), Qt::CaseInsensitive) == 0);

		// If key exists, but doesn't match - update it
		if (!keyMatches)
			settings.setValue(APP_NAME, getOnComputerStartStartString());
	}

	settings.endGroup();

	return keyExists;
}

void  ProgramSettings::setIsStartingWhenComputerStarts(bool value)
{
	QSettings settings("Microsoft", "Windows");
	settings.beginGroup("CurrentVersion/Run");

	if (value)
		settings.setValue(APP_NAME, getOnComputerStartStartString());
	else
		settings.remove(APP_NAME);

	settings.endGroup();
}

QString  ProgramSettings::getOnComputerStartStartString()
{
	return QString("\"%1\" --silentstart").arg(QDir::toNativeSeparators(AppTools::getPathToExe()));
}

#endif

#ifdef Q_WS_MAC
// TODO FUNC Cross impl
bool ProgramSettings::getIsStartingWhenComputerStarts()
{
    return MacTools::getAutoStart();
}

void ProgramSettings::setIsStartingWhenComputerStarts(bool value)
{
    MacTools::setAutoStart(value);
}

QString ProgramSettings::getOnComputerStartStartString()
{
	// TODOMAC
	return QString();
}

#endif

#ifdef Q_WS_X11
// TODO FUNC Cross impl
bool ProgramSettings::getIsStartingWhenComputerStarts()
{
    return false;
}

void ProgramSettings::setIsStartingWhenComputerStarts(bool value)
{
    //TODO LINUX
}

QString ProgramSettings::getOnComputerStartStartString()
{
	// TODO Linux
	return QString();
}

#endif

// NOTE only first call to this function is correct!
bool ProgramSettings::getIsFirstAppStart()
{
	bool result = true;

	QSettings settings;
	settings.beginGroup(APP_OPTION_GROUP);

	result = settings.value(STARTED_BEFORE_OPTION).isNull();

	if (result)
	{
		settings.setValue(STARTED_BEFORE_OPTION, true);
		settings.setValue(PLAY_SOUNDS_OPTION, true);
	}

	settings.endGroup();

	return result;
}

void ProgramSettings::registerUrlProtocol()
{
#ifdef WIN32
	// Protocol
	QString path("HKEY_CURRENT_USER\\Software\\Classes\\"URL_PROTOCOL_NAME);

	QSettings settings(path, QSettings::NativeFormat);

	QString defaultValue = "Default";

	QVariant key = settings.value(defaultValue);

	if (!key.isNull())
		return;

	settings.setValue(defaultValue, "URL:MoodBox Protocol");
	settings.setValue("URL Protocol", "");

	// Icon
	settings.beginGroup("DefaultIcon");
	QString appPath = "\"" + QDir::toNativeSeparators(AppTools::getPathToExe()) + "\"";

	settings.setValue(defaultValue, appPath);
	settings.endGroup();

	// Open command
	settings.beginGroup("shell");
	settings.beginGroup("open");
	settings.beginGroup("command");

	QString command = appPath + " \"%1\"";
	settings.setValue(defaultValue, command);
#else
	//TODOMAC
	//not implemented!
#endif
}

bool ProgramSettings::getPlaySouns()
{
	QSettings settings;
	settings.beginGroup(APP_OPTION_GROUP);

	bool result = settings.value(PLAY_SOUNDS_OPTION).toBool();

	settings.endGroup();

	return result;
}

}
