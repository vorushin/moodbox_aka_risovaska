#ifndef TOOLSETTINGSBAR_H
#define TOOLSETTINGSBAR_H

#include <QWidget>

namespace Velasquez
{
	class SettingsProvider;
}

namespace MoodBox
{

// Setting id for shared colors
#define SHARED_COLORINDEX_SETTING		101

// Basic tool setting widget
class ToolSettingsBar : public QWidget
{
	Q_OBJECT

public:
	ToolSettingsBar(QWidget *parent = 0);

	void setSettingsProvider(Velasquez::SettingsProvider *settings);
	inline Velasquez::SettingsProvider *getSettingsProvider() const { return settings; };

	virtual void reset() = 0;

protected:
	Velasquez::SettingsProvider *settings;

	virtual void updateSetting(qint32 id);

private slots:
	void onSettingChanged(qint32 id);
};

}

#endif // TOOLSETTINGSBAR_H
