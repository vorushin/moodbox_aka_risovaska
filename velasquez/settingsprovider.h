#ifndef SETTINGSPROVIDER_H
#define SETTINGSPROVIDER_H

#include <QObject>

#include "varianthash.h"

namespace Velasquez
{

// Generic settings collection with notifications upon item value change
// Settings providers can connect to each other to share settings via external settings mechanism
class SettingsProvider: public QObject, VariantHash
{
	Q_OBJECT

public:
    SettingsProvider(QObject *parent = NULL);

	// General operations
	void setSetting(qint32 id, const QVariant &value);
	QVariant getSetting(qint32 id) const;
	void removeSetting(qint32 id);
	bool hasSetting(qint32 id) const;

	// External settings operations
	void includeExternalSetting(qint32 id, SettingsProvider* provider);
	void excludeExternalSetting(qint32 id);
	bool isSettingExternal(qint32 id) const;

	// Connect object to provider's settingChanged signal to onSettingChanged slot with disconnect of old provider
	static bool connectSettings(QObject *target, SettingsProvider *oldSettings, SettingsProvider *newSettings);

signals:
	void settingChanged(qint32 id);

private slots:
	void onExternalSettingChanged(qint32 id);

private:
	QHash <qint32, SettingsProvider*> externalSettings;
    
};

}

#endif // SETTINGSPROVIDER_H
