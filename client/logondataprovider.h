#ifndef LOGONDATAPROVIDER_H
#define LOGONDATAPROVIDER_H

#include <QHash>
#include <QByteArray>
#include <QStringList>
#include <QSettings>

#include "userstatus.h"

namespace MoodBox
{

// LogonDataProvider
#define AUTH_OPTION_GROUP					"Authorization"
#define LOGIN_OPTION						"Login"
#define LOGONDATA_OPTION					"Data"
#define IS_PASSWORD_SAVING_ENABLED_OPTION	"IsPasswordSavingEnabled"
#define LAST_LOGGEDON_LOGIN_OPTION			"LastLoggedOnLogin"
#define IS_AUTOLOGON_ENABLED_OPTION			"IsAutoLogonEnabled"

// ProgramLogonProvider
#define PROGRAM_LOGINDATA_SUBGROUP			"LoginData"
#define STATUS_OPTION						"Status"

// ServicesLoginProvider
#define SERVICES_LOGINDATA_SUBGROUP			"LoginData2"
#define SERVICE_OPTION						"S2"
#define SERVICE_LJ_NAME						"GLO"
#define SERVICE_LI_NAME						"LI"
#define SERVICE_DIARY_NAME					"DI"
#define SERVICE_MAILRU_NAME					"MR"

#define LOGONPROVIDER						ProgramDataProvider::getInstance()
#define SERVICELOGONPROVIDER				ServiceDataProvider::getInstance()

// Login/Password encrypted data storage
class LogonDataProvider
{
public:
	virtual ~LogonDataProvider() {};
	
	// settings groups
	inline virtual QString getMainSubgroup() const { return AUTH_OPTION_GROUP; };
	virtual QString getSettingsSubgroup() const = 0;
	
	// connected directly to checkbox in settings
	bool getIsAutoLogonEnabled() const;
	virtual void setIsAutoLogonEnabled(bool value);

	// checks all necessary conditions, encluding is autoLogon enabled
	virtual bool isAutoLogonPossible() const;

	// for change password dialog
	void applyNewPassword(const QString &password);

	// current logon and password to use for logon and relogon
	QString getCurrentLogin() const;
	QString getCurrentPassword() const;

	// to set login and password to use before starting logon
	void setCurrent(const QString &login, const QString &password);

	// for LogonWidget - what list item should be shown at start
	QString getLastLoggedOnLogin() const;

	// connected directly to checkbox in settings
	bool getIsPasswordSavingEnabled() const;
	virtual void setIsPasswordSavingEnabled(bool value);

	// to fill list of logins used
	QStringList getSavedLogins() const;

	// must be called on succesfull logon to remember login and password if corresponding option set
	void saveLogonDataIfAllowed();

	// get saved password for login
	QString getPassword(const QString &login) const;

	// reloads data from config (registry on Windows)
	virtual void reload();

protected:
	LogonDataProvider();
	
	virtual bool storeOptionsGlobally() const { return true; };
	virtual bool removeNotLoadedItems() const { return true; };

	virtual void init();

	void beginOptionsGroup(QSettings &settings);
	void endOptionsGroup(QSettings &settings);

	virtual bool loadItemInfo(QSettings &settings, const QString &name, const QString &login);
	virtual void loadOptions(QSettings &settings);

	virtual bool isFoundItem(QSettings &settings, const QString &item, const QString &idKey, const QString &idValue, bool *isEmpty = NULL);
	virtual void beginAddItemToSettings(QSettings &settings, const QString &item, const QString &idKey, const QString &idValue);

	virtual void removeSettingsSetItem(QSettings &settings, const QString &idKey, const QString &idValue);
	virtual void beginAddSetItemToSettings(QSettings &settings, const QString &idKey, const QString &idValue);

private:
	QString currentLogin;
	QByteArray currentLogonData;

	bool isAutoLogonEnabled;
	bool isPasswordSavingEnabled;

	QString lastLoggedOnLogin;
	QHash<QString, QByteArray> logonData;

	QByteArray getCurrentLogonData() const;
	void setCurrent(const QString &login);
	QByteArray getLogonData(const QString &login) const;
	QString getPassword(const QByteArray &encryptedData) const;
	QByteArray encodePassword(const QString &password);

	void savePassword(const QString &login, const QString &password);
	void saveLogonData(const QString &login, const QByteArray &data);
	void saveLastLoggedOnLogin(const QString &login);

	void removeLogonData(const QString &login);
	void resetLastLoggedOnLogin();
};

// ProgramDataProvider keeps MoodBox users data and status
class ProgramDataProvider : public LogonDataProvider
{
public:
	static ProgramDataProvider* getInstance();

	inline virtual QString getSettingsSubgroup() const { return PROGRAM_LOGINDATA_SUBGROUP; };

	virtual bool isAutoLogonPossible() const;

	// for setting desired status just after logon (unused from outer code for now)
	UserStatus::UserStatusEnum getSavedStatus(const QString &login) const;
	// save choosen status to be able to restore it in future - must be set on status change and got in account when trying to logon if logging on in progress
	void saveStatus(UserStatus::UserStatusEnum status, bool preventPersisting = false);

	virtual void reload();

protected:
	virtual bool loadItemInfo(QSettings &settings, const QString &name, const QString &login);

private:
	QHash<QString, UserStatus::UserStatusEnum> logonStatus;

	static ProgramDataProvider* programLoginInstance;
	
	void saveStatus(const QString &login, UserStatus::UserStatusEnum status, bool preventPersisting = false);
};

// ServiceDataProvider keeps MoodBox users data for external services, such as blogs
class ServiceDataProvider : public LogonDataProvider
{
public:
	static ServiceDataProvider* getInstance();

	inline virtual QString getSettingsSubgroup() const { return SERVICES_LOGINDATA_SUBGROUP; };

	QString getCurrentService() const { return currentService;} ;
	void setCurrentService(const QString &serviceName);
	
	QStringList getServicesList() const { return services; };

protected:
	inline virtual bool storeOptionsGlobally() const { return false; };
	virtual bool removeNotLoadedItems() const { return false; };

	virtual bool loadItemInfo(QSettings &settings, const QString &name, const QString &login);

	virtual bool isFoundItem(QSettings &settings, const QString &item, const QString &idKey, const QString &idValue, bool *isEmpty = NULL);
	virtual void beginAddItemToSettings(QSettings &settings, const QString &item, const QString &idKey, const QString &idValue);

private:
	QString currentService;
	QStringList services;

	static ServiceDataProvider* serviceLoginInstance;
};

}

#endif // LOGONDATAPROVIDER_H
