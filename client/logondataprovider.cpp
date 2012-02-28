#include "logondataprovider.h"

#include <QBuffer>

#include "encryption.h"

namespace MoodBox
{

// LogonDataProvider class
bool LogonDataProvider::getIsAutoLogonEnabled() const
{
	return isAutoLogonEnabled;
}

void LogonDataProvider::setIsAutoLogonEnabled(bool value)
{
	isAutoLogonEnabled = value;

	QSettings settings;
	
	beginOptionsGroup(settings);
	settings.setValue(IS_AUTOLOGON_ENABLED_OPTION, value);	
	endOptionsGroup(settings);
}

bool LogonDataProvider::isAutoLogonPossible() const
{
	return getIsAutoLogonEnabled() && !lastLoggedOnLogin.isEmpty() && !getPassword(lastLoggedOnLogin).isEmpty();
}

void LogonDataProvider::applyNewPassword(const QString &password)
{
	currentLogonData = encodePassword(password);
	saveLogonDataIfAllowed();
}

QString LogonDataProvider::getCurrentLogin() const
{
	return currentLogin;
}

QString LogonDataProvider::getCurrentPassword() const 
{
	return getPassword(getCurrentLogonData());
}

void LogonDataProvider::setCurrent(const QString &login, const QString &password)
{
	currentLogin = login;
	currentLogonData = encodePassword(password);
}

QString LogonDataProvider::getLastLoggedOnLogin() const
{
	return lastLoggedOnLogin;
}

bool LogonDataProvider::getIsPasswordSavingEnabled() const
{
	return isPasswordSavingEnabled;
}

void LogonDataProvider::setIsPasswordSavingEnabled(bool value)
{
	isPasswordSavingEnabled = value;

	QSettings settings;

	beginOptionsGroup(settings);
	settings.setValue(IS_PASSWORD_SAVING_ENABLED_OPTION, value);
	endOptionsGroup(settings);
}

QStringList LogonDataProvider::getSavedLogins() const
{
	return logonData.keys();
}

void LogonDataProvider::saveLogonDataIfAllowed()
{
	QString login = currentLogin;

	if (login.isEmpty())
		return;

	if (!getIsPasswordSavingEnabled())
	{
		resetLastLoggedOnLogin();
		removeLogonData(login);
	}
	else
	{
		saveLastLoggedOnLogin(login);
		saveLogonData(login, currentLogonData);
	}
}

QString LogonDataProvider::getPassword(const QString &login) const
{
	return getPassword(getLogonData(login));
}

void LogonDataProvider::reload()
{
	logonData.clear();

	QSettings settings;
	settings.beginGroup(getMainSubgroup());
	settings.beginGroup(getSettingsSubgroup());

	foreach(QString item, settings.childGroups())
	{
		settings.beginGroup(item);

		QString login = settings.value(LOGIN_OPTION).toString();
		bool loaded = false;

		if (!login.isEmpty())
		{
			loaded = loadItemInfo(settings, item, login);			
		}

		settings.endGroup();

		if (!loaded && removeNotLoadedItems())
			settings.remove(item);
	}

	if (storeOptionsGlobally())
		settings.endGroup();

	loadOptions(settings);

	settings.endGroup();

	if (!storeOptionsGlobally())
		settings.endGroup();
}

LogonDataProvider::LogonDataProvider()
{
}

void LogonDataProvider::init()
{
	reload();

	setCurrent(getLastLoggedOnLogin());
}

void LogonDataProvider::beginOptionsGroup(QSettings &settings)
{
	settings.beginGroup(getMainSubgroup());	
	
	if (!storeOptionsGlobally())
		settings.beginGroup(getSettingsSubgroup());	
}

void LogonDataProvider::endOptionsGroup(QSettings &settings)
{
	if (!storeOptionsGlobally())
		settings.endGroup();

	settings.endGroup();
}

bool LogonDataProvider::loadItemInfo(QSettings &settings, const QString &name, const QString &login)
{
	Q_UNUSED(name)

	QByteArray data = settings.value(LOGONDATA_OPTION).toByteArray();

	// prevent data without passwords
	if (!getPassword(data).isEmpty())
	{
		logonData.insert(login, data);

		return true;
	}
	else
		return false;
}

void LogonDataProvider::loadOptions(QSettings &settings)
{
	lastLoggedOnLogin = settings.value(LAST_LOGGEDON_LOGIN_OPTION).toString();
	isAutoLogonEnabled = settings.value(IS_AUTOLOGON_ENABLED_OPTION, true).toBool();
	isPasswordSavingEnabled = settings.value(IS_PASSWORD_SAVING_ENABLED_OPTION, true).toBool();
}

bool LogonDataProvider::isFoundItem(QSettings &settings, const QString &item, const QString &idKey, const QString &idValue, bool *isEmpty)
{
	settings.beginGroup(item);
	QString value = settings.value(idKey).toString();
	settings.endGroup();

	if (isEmpty != NULL)
		*isEmpty = value.isEmpty();

	return (value == idValue || value.isEmpty());
}

void LogonDataProvider::beginAddItemToSettings(QSettings &settings, const QString &item, const QString &idKey, const QString &idValue)
{
	settings.beginGroup(item);
	settings.setValue(idKey, idValue);
}

void LogonDataProvider::removeSettingsSetItem(QSettings &settings, const QString &idKey, const QString &idValue)
{
	foreach(QString item, settings.childGroups())
	{
		if (isFoundItem(settings, item, idKey, idValue))
			settings.remove(item);
	}
}

void LogonDataProvider::beginAddSetItemToSettings(QSettings &settings, const QString &idKey, const QString &idValue)
{
	QString item, foundItem;

	bool wasEmpty;

	foreach(item, settings.childGroups())
	{
		if (isFoundItem(settings, item, idKey, idValue, &wasEmpty))
		{
			if (!foundItem.isEmpty())
				settings.remove(foundItem); // remove previously found duplicate

			foundItem = item;
		}

		if (wasEmpty)
			settings.remove(item);
	}

	if (foundItem.isEmpty())
	{
		for (quint32 i = 0; i < 5000; i++)
		{
			QString itemId = QString::number(i);

			isFoundItem(settings, itemId, idKey, idValue, &wasEmpty);

			// Replate empty item
			if (wasEmpty)
			{
				settings.remove(itemId);
				beginAddItemToSettings(settings, itemId, idKey, idValue);

				return;
			}
		}

		// couldn't find empty slot, replace first item
		item = settings.childGroups().at(0);
		settings.remove(item);
		beginAddItemToSettings(settings, item, idKey, idValue);
	}
	else
		settings.beginGroup(foundItem);
}

QByteArray LogonDataProvider::getCurrentLogonData() const
{
	return currentLogonData;
}

void LogonDataProvider::setCurrent(const QString &login)
{
	currentLogin = login;
	currentLogonData = getLogonData(login);
}

QByteArray LogonDataProvider::getLogonData(const QString &login) const
{
	return logonData.value(login);
}

QString LogonDataProvider::getPassword(const QByteArray &encryptedData) const
{
	if (encryptedData.size() == 0)
		return QString();

	QByteArray plainData;
	Encryptor::decrypt(encryptedData, plainData);

	QBuffer buffer(&plainData);
	buffer.open(QIODevice::ReadOnly);

	QDataStream stream(&buffer);
	
	QString password;
	stream >> password;
	
	plainData.clear();

	return password;
}

QByteArray LogonDataProvider::encodePassword(const QString &password)
{
	QByteArray result;
	QByteArray plainData;	

	QBuffer buffer(&plainData);
	buffer.open(QIODevice::WriteOnly);
	
	QDataStream stream(&buffer);
	stream << password;

	Encryptor::encrypt(plainData, result);

	plainData.clear();

	return result;
}

void LogonDataProvider::savePassword(const QString &login, const QString &password)
{
	saveLogonData(login, encodePassword(password));
}

void LogonDataProvider::saveLogonData(const QString &login, const QByteArray &data)
{
	logonData.insert(login, data);

	QSettings settings;
	settings.beginGroup(getMainSubgroup());
	settings.beginGroup(getSettingsSubgroup());

	beginAddSetItemToSettings(settings, LOGIN_OPTION, login);
	settings.setValue(LOGONDATA_OPTION, data);
	settings.endGroup();

	settings.endGroup();
	settings.endGroup();
}

void LogonDataProvider::saveLastLoggedOnLogin(const QString &login)
{
	QSettings settings;

	beginOptionsGroup(settings);
	settings.setValue(LAST_LOGGEDON_LOGIN_OPTION, login);
	endOptionsGroup(settings);
}

void LogonDataProvider::removeLogonData(const QString &login)
{
	logonData.remove(login);

	QSettings settings;
	settings.beginGroup(getMainSubgroup());
	settings.beginGroup(getSettingsSubgroup());
	removeSettingsSetItem(settings, LOGIN_OPTION, login);
	settings.endGroup();
	settings.endGroup();
}

void LogonDataProvider::resetLastLoggedOnLogin()
{
	lastLoggedOnLogin = QString();

	QSettings settings;
	beginOptionsGroup(settings);
	settings.remove(LAST_LOGGEDON_LOGIN_OPTION);
	endOptionsGroup(settings);
}

// ProgramDataProvider class
ProgramDataProvider *ProgramDataProvider::programLoginInstance = NULL;
ProgramDataProvider *ProgramDataProvider::getInstance()
{
	if (programLoginInstance == NULL)
	{
		programLoginInstance = new ProgramDataProvider();
		programLoginInstance->init();
	}

	return programLoginInstance;
}

bool ProgramDataProvider::isAutoLogonPossible() const
{
	return LogonDataProvider::isAutoLogonPossible() && (getSavedStatus(getLastLoggedOnLogin()) != UserStatus::Offline);
}

UserStatus::UserStatusEnum ProgramDataProvider::getSavedStatus(const QString &login) const
{
	return logonStatus.value(login);
}

void ProgramDataProvider::saveStatus(UserStatus::UserStatusEnum status, bool preventPersisting)
{
	saveStatus(getCurrentLogin(), status, preventPersisting);
}

void ProgramDataProvider::reload()
{
	logonStatus.clear();

	LogonDataProvider::reload();
}

bool ProgramDataProvider::loadItemInfo(QSettings &settings, const QString &name, const QString &login)
{
	if (!LogonDataProvider::loadItemInfo(settings, name, login))
		return false;

	UserStatus::UserStatusEnum status = (UserStatus::UserStatusEnum)settings.value(STATUS_OPTION, UserStatus::Offline).toInt();

	logonStatus.insert(login, status);

	return true;
}

void ProgramDataProvider::saveStatus(const QString &login, UserStatus::UserStatusEnum status, bool preventPersisting)
{
	Q_ASSERT_X(status != UserStatus::Connecting, "LogonDataProvider::saveStatus", "attempt to save Connecting status");
	Q_ASSERT_X(status != UserStatus::Undefined, "LogonDataProvider::saveStatus", "attempt to save Unknown status");
	Q_ASSERT_X(!login.isEmpty(), "LogonDataProvider::saveStatus", "attempt to save status when no current user set");

	logonStatus.insert(login, status);

	if(!preventPersisting && getIsPasswordSavingEnabled())
	{
		QSettings settings;
		settings.beginGroup(getMainSubgroup());
		settings.beginGroup(getSettingsSubgroup());

		beginAddSetItemToSettings(settings, LOGIN_OPTION, login);
		settings.setValue(STATUS_OPTION, status);
		settings.endGroup();

		settings.endGroup();
		settings.endGroup();
	}
}

// ServiceDataProvider class
ServiceDataProvider *ServiceDataProvider::serviceLoginInstance = NULL;
ServiceDataProvider *ServiceDataProvider::getInstance()
{
	if (serviceLoginInstance == NULL)
	{
		serviceLoginInstance = new ServiceDataProvider();
		serviceLoginInstance->init();
	}

	return serviceLoginInstance;
}

void ServiceDataProvider::setCurrentService(const QString &serviceName)
{
	if (currentService == serviceName)
		return;

	currentService = serviceName;
}

bool ServiceDataProvider::loadItemInfo(QSettings &settings, const QString &name, const QString &login)
{
	QString service = settings.value(SERVICE_OPTION, QString()).toString();

	if (!services.contains(service) && !service.isEmpty())
		services.append(service);

	if (currentService.isEmpty())
		setCurrentService(service);

	if (service == getCurrentService())
		return LogonDataProvider::loadItemInfo(settings, name, login);
	else
		return false;
}

bool ServiceDataProvider::isFoundItem(QSettings &settings, const QString &item, const QString &idKey, const QString &idValue, bool *isEmpty)
{
	settings.beginGroup(item);
	QString value = settings.value(idKey).toString();
	QString service = settings.value(SERVICE_OPTION).toString();
	settings.endGroup();

	bool empty = value.isEmpty() || service.isEmpty();
	if (isEmpty != NULL)
		*isEmpty = empty;

	return ( (value == idValue && service == currentService) || empty);
}

void ServiceDataProvider::beginAddItemToSettings(QSettings &settings, const QString &item, const QString &idKey, const QString &idValue)
{
	LogonDataProvider::beginAddItemToSettings(settings, item, idKey, idValue);
	settings.setValue(SERVICE_OPTION, currentService);	
}

}