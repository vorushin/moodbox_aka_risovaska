#include "toolsettingsbar.h"

#include "settingsprovider.h"
#include "testtools.h"

namespace MoodBox
{

using namespace Velasquez;

ToolSettingsBar::ToolSettingsBar(QWidget *parent)
	: QWidget(parent), settings(NULL)
{
	TimeMeasure t("ToolSettingsBar");

	setSettingsProvider(new SettingsProvider(this));
}

void ToolSettingsBar::setSettingsProvider(Velasquez::SettingsProvider *settings)
{
	if (this->settings == settings)
		return;

	if (this->settings != NULL)
		disconnect(this->settings, SIGNAL(settingChanged(qint32)), this, SLOT(onSettingChanged(qint32)));

	this->settings = settings;

	if (this->settings != NULL)
		connect(this->settings, SIGNAL(settingChanged(qint32)), this, SLOT(onSettingChanged(qint32)));
}

void ToolSettingsBar::updateSetting(qint32 id)
{
	Q_UNUSED(id)
}

void ToolSettingsBar::onSettingChanged(qint32 id)
{
	updateSetting(id);
}

}
