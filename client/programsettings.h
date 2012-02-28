#ifndef PROGRAMSETTINGS_H
#define PROGRAMSETTINGS_H

#include <QNetworkProxy>

namespace MoodBox
{

#define DEFAULT_SERVER_PORT						5190
#define ALTERNATE_SERVER_PORT					443

#define DEFAULT_SOCKS_PROXY_PORT				1080
#define DEFAULT_HTTP_PROXY_PORT					80

#define APP_OPTION_GROUP						"Application"
#define STARTED_BEFORE_OPTION					"StartedBefore"
#define PLAY_SOUNDS_OPTION						"PlaySounds"

#define URL_PROTOCOL_NAME						"moodbox"
#define ADD_FRIEND_COMMAND						"addfriend"
#define SINGLE_APP_MESSAGE						"Do_I_exist?"

#define NETWORK_SETTINGS_GROUP					"Network"
#define SERVER_PORT_OPTION						"ServerPort"

#define PROXY_SETTINGS_SUBGROUP					"Proxy"
#define USE_PROXY_OPTION						"UseProxy"
#define PROXY_TYPE_OPTION						"Type"
#define PROXY_HOST_OPTION						"Host"
#define PROXY_PORT_SOCKS_OPTION					"Port_Socks"
#define PROXY_PORT_HTTP_OPTION					"Port_Http"
#define PROXY_REQUIRES_AUTHORIZATION_OPTION		"RequiresAuthorization"
#define PROXY_USER_NAME_OPTION					"UserName"
#define PROXY_PASSWORD_OPTION					"Password"

#define SERVER_SETTINGS_FILE					"server.ini"
#define DEFAULT_SERVER_URL						"https://server.moodbox.com:443/do"

// Class for store/read program settings
class ProgramSettings
{
public:
	ProgramSettings();

	int getProxyPort() const;

	void readSettings();
	void writeSettings() const;

	void apply();

	static void applyProxySettings();
	static QNetworkProxy getProxySettings(const ProgramSettings &programSettings);

	static QString getDefaultSocksProxyPort();
	static QString getDefaultHttpProxyPort();
	static bool getPlaySouns();

	// Check does app start when computer starts
	static bool getIsStartingWhenComputerStarts();

	// Set app start when computer starts
	static void setIsStartingWhenComputerStarts(bool value);

	// Is it first application start
	static bool getIsFirstAppStart();

	// Register url protocol for browsers
	static void registerUrlProtocol();

public:
	bool autoLaunchProgram;

	bool playSounds;

	QString serverUrl;
	int serverPort;

	bool useProxy;
	QNetworkProxy::ProxyType proxyType;
	QString proxyHost;
	QString proxyPortSocks;
	QString proxyPortHttp;

	bool proxyRequiresAuthorization;
	QString proxyUserName;
	QString proxyPassword;

private:
	static QString getOnComputerStartStartString();
};

}

#endif // PROGRAMSETTINGS_H
