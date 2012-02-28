#ifndef BRUSHSETTINGSBAR_H
#define BRUSHSETTINGSBAR_H

#include "toolsettingsbar.h"

#include "ui_brushsettingsbar.h"

namespace MoodBox
{

using namespace Ui;

// Pen/brush settings bar
class BrushSettingsBar : public ToolSettingsBar, public BrushSettingsBarClass
{
	Q_OBJECT

public:
	BrushSettingsBar(QWidget *parent = 0);

	virtual void reset();

public slots:
	void changeSelectedColor(const QColor &color);

protected:
	// Access to controls for children
	PaletteWidget2 *getPaletteWidget() const { return paletteWidget; };
	QToolButton *getPaletteManagerButton() const { return paletteManagerButton; };
	BrushSizeButton *getSizeButton() const { return sizeButton; };
	BrushAlphaButton *getAlphaButton() const { return alphaButton; };
	
	virtual void updateSetting(qint32 id);

private slots:
	void onColorSelected(qint32 index);
	void onColorChanged(qint32 index, const QColor &newColor);

	void onSizeSelected(int sizeIndex);
	void onAlphaSelected(int alphaIndex);

	void onPaletteSelected(const Palette &palette);

	void on_paletteManagerButton_toggled(bool checked);
	void onPaletteManagerFinished();
};

}

#endif // BRUSHSETTINGSBAR_H
