#include "brushsettingsbar.h"

#include "palettemanager.h"
#include "mousedrawingelement.h"
#include "brushstyle.h"
#include "settingsprovider.h"

#include "mousedrawingelement.h"
#include "testtools.h"
#include "debug.h"

namespace MoodBox
{

using namespace Velasquez;

BrushSettingsBar::BrushSettingsBar(QWidget *parent)
	: ToolSettingsBar(parent)
{
	TimeMeasure t("BrushSettingsBar");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	paletteWidget->setPalette(PALETTEMANAGER->getDefaultPalette());

	// Defaults
	QColor defaultColor = paletteWidget->getPalette().getColor(0);
	paletteWidget->setSelectedColorIndex(0);

	sizeButton->setSizeIndex(DEFAULT_BRUSH_SIZE_INDEX);
	sizeButton->setColor(defaultColor);

	alphaButton->setSizeIndex(DEFAULT_BRUSH_SIZE_INDEX);
	alphaButton->setAlphaIndex(DEFAULT_BRUSH_ALPHA_INDEX);

	onColorSelected(0);
	onSizeSelected(DEFAULT_BRUSH_SIZE_INDEX);
	onAlphaSelected(DEFAULT_BRUSH_ALPHA_INDEX);

	// Connections
	connect(paletteWidget, SIGNAL(colorSelected(qint32)), this, SLOT(onColorSelected(qint32)));
	connect(paletteWidget, SIGNAL(colorChanged(qint32, const QColor &)), this, SLOT(onColorChanged(qint32, const QColor &)));

	connect(sizeButton, SIGNAL(sizeSelected(int)), alphaButton, SLOT(setSizeIndex(int)));
	
	connect(sizeButton, SIGNAL(sizeSelected(int)), this, SLOT(onSizeSelected(int)));
	connect(alphaButton, SIGNAL(alphaSelected(int)), this, SLOT(onAlphaSelected(int)));

	connect(PALETTEMANAGER, SIGNAL(paletteSelected(const Palette &)), this, SLOT(onPaletteSelected(const Palette &)));
	connect(PALETTEMANAGER, SIGNAL(finished()), this, SLOT(onPaletteManagerFinished()));
}

void BrushSettingsBar::reset()
{
	paletteWidget->reset();
	paletteWidget->setSelectedColorIndex(0);

	sizeButton->setSizeIndex(DEFAULT_BRUSH_SIZE_INDEX);

	if (paletteWidget->isEnabled())
		sizeButton->setColor(paletteWidget->getPalette().getColor(0));

	alphaButton->setSizeIndex(DEFAULT_BRUSH_SIZE_INDEX);
	alphaButton->setAlphaIndex(DEFAULT_BRUSH_ALPHA_INDEX);
}

void BrushSettingsBar::changeSelectedColor(const QColor &color)
{
	if (getPaletteWidget() != NULL)
		getPaletteWidget()->updateSelectedColor(color);
}

void BrushSettingsBar::updateSetting(qint32 id)
{
	if (id == SHARED_COLORINDEX_SETTING)
	{
		qint32 newColorIndex = settings->getSetting(id).value<qint32>();

		paletteWidget->setSelectedColorIndex(newColorIndex);

		QColor newColor = paletteWidget->getSelectedColor();

		if (!newColor.isValid())
			return;

		settings->setSetting(MouseDrawingElement::PenColor, newColor);
	}
	else
		if (id == MouseDrawingElement::PenColor)
		{
			if (paletteWidget->isEnabled())
				sizeButton->setColor(settings->getSetting(MouseDrawingElement::PenColor).value<QColor>() );
		}
}

void BrushSettingsBar::onColorSelected(qint32 index)
{
	if (index == settings->getSetting(SHARED_COLORINDEX_SETTING).toInt())
		return;

	settings->setSetting(SHARED_COLORINDEX_SETTING, index);
}

void BrushSettingsBar::onColorChanged(qint32 index, const QColor &newColor)
{
	PALETTEMANAGER->setPalette(paletteWidget->getPalette());

	if (index == paletteWidget->getSelectedColorIndex())
		settings->setSetting(MouseDrawingElement::PenColor, newColor);
}

void BrushSettingsBar::onSizeSelected(int sizeIndex)
{
	// Elements and settings do not know anything about indexes, etc, so we just convert it from 1 to index count
	settings->setSetting(MouseDrawingElement::PenWidth, sizeIndex + 1);
}

void BrushSettingsBar::onAlphaSelected(int alphaIndex)
{
	settings->setSetting(MouseDrawingElement::PenTransparency, alphaIndex);
}

void BrushSettingsBar::onPaletteSelected(const Palette &palette)
{
	if (palette == paletteWidget->getPalette())
		return;

	paletteWidget->setPalette(palette);	

	if (paletteWidget->getSelectedColorIndex() < 0)
		return;

	settings->setSetting(MouseDrawingElement::PenColor, paletteWidget->getSelectedColor());
}

void BrushSettingsBar::on_paletteManagerButton_toggled(bool checked)
{
	if (checked)
		PALETTEMANAGER->managePalette(paletteWidget->getPalette(), paletteManagerButton->mapToGlobal(QPoint(paletteManagerButton->width() - 4, 0)));
}

void BrushSettingsBar::onPaletteManagerFinished()
{
	if (paletteManagerButton->isChecked())
		paletteManagerButton->toggle();
}

}
