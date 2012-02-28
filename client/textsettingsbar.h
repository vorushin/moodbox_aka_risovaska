#ifndef TEXTSETTINGSBAR_H
#define TEXTSETTINGSBAR_H

#include "toolsettingsbar.h"

#include "ui_textsettingsbar.h"

namespace MoodBox
{

using namespace Ui;

// Text settings tool bar
class TextSettingsBar : public ToolSettingsBar, public TextSettingsBarClass
{
	Q_OBJECT

public:
	TextSettingsBar(QWidget *parent = 0);

	virtual void reset();

protected:
	virtual void updateSetting(qint32 id);

private:
	int fontStyle;
	void applyFontStyle();

private slots:
	void onColorSelected(qint32 index);
	void onColorChanged(qint32 index, const QColor &newColor);

	void onFontNameSelected(const QString &fontName);
	void onPaletteSelected(const Palette &palette);

	void on_boldButton_clicked();
	void on_italicButton_clicked();
	void on_underlineButton_clicked();

	void on_paletteManagerButton_toggled(bool checked);
	void onPaletteManagerFinished();
};

}

#endif // TEXTSETTINGSBAR_H
