#include "settingsprovider.h"

namespace Velasquez
{

SettingsProvider::SettingsProvider(QObject *parent)
	: QObject(parent)
{
}

void SettingsProvider::setSetting(qint32 id, const QVariant &value)
{
	if (isSettingExternal(id))
	{
		externalSettings[id]->setSetting(id, value);

		return;
	}
	else
		setData(id, value);

	emit settingChanged(id);
}

QVariant SettingsProvider::getSetting(qint32 id) const
{
	if (isSettingExternal(id))
		return externalSettings[id]->getSetting(id);
	else
		return getData(id);
}

void SettingsProvider::removeSetting(qint32 id)
{
	if (isSettingExternal(id))
		excludeExternalSetting(id);
	else
		removeData(id);
}

bool SettingsProvider::hasSetting(qint32 id) const
{
	return hasData(id) || externalSettings.contains(id);
}

void SettingsProvider::includeExternalSetting(qint32 id, SettingsProvider* provider)
{
	if (isSettingExternal(id))
		return;

	// we do not need to keep an old one
	removeSetting(id);

	externalSettings[id] = provider;
	connect(provider, SIGNAL(settingChanged(qint32)), this, SLOT(onExternalSettingChanged(qint32)));
	
	// send update
	onExternalSettingChanged(id);
}

void SettingsProvider::excludeExternalSetting(qint32 id)
{
	if (!isSettingExternal(id))
		return;
	
	// get current value to local use
	QVariant syncValue = getSetting(id);
	disconnect(externalSettings[id], SIGNAL(settingChanged(qint32)), this, SLOT(onExternalSettingChanged(qint32)));

	externalSettings.remove(id);
	setSetting(id, syncValue);
}

bool SettingsProvider::isSettingExternal(qint32 id) const
{
	return externalSettings.contains(id);
}

bool SettingsProvider::connectSettings(QObject *target, SettingsProvider *oldSettings, SettingsProvider *newSettings)
{
	bool goodConnect = false;

	if (oldSettings != NULL)
		goodConnect = QObject::disconnect(oldSettings, SIGNAL(settingChanged(qint32)), target, SLOT(onSettingChanged(qint32)));

	if (newSettings != NULL)
		goodConnect = QObject::connect(newSettings, SIGNAL(settingChanged(qint32)), target, SLOT(onSettingChanged(qint32)));
	
	return goodConnect;
}

void SettingsProvider::onExternalSettingChanged(qint32 id)
{
	if (isSettingExternal(id))
		emit settingChanged(id);
}

}