#include "textsettingsbar.h"

#include "palettemanager.h"
#include "textelement.h"
#include "settingsprovider.h"

#include "testtools.h"
#include "debug.h"

namespace MoodBox
{

using namespace Velasquez;

TextSettingsBar::TextSettingsBar(QWidget *parent)
	: ToolSettingsBar(parent), fontStyle(0)
{
	TimeMeasure t("TextSettingsBar");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	paletteWidget->setPalette(PALETTEMANAGER->getDefaultPalette());

	connect(paletteWidget, SIGNAL(colorSelected(qint32)), this, SLOT(onColorSelected(qint32)));
	connect(paletteWidget, SIGNAL(colorChanged(qint32, const QColor &)), this, SLOT(onColorChanged(qint32, const QColor &)));

	connect(styleButton, SIGNAL(fontNameSelected(const QString &)), this, SLOT(onFontNameSelected(const QString &)));

	// Defaults
	paletteWidget->setSelectedColorIndex(0);
	paletteWidget->setRequireNewColor(false);

	onColorSelected(0);
	onFontNameSelected(styleButton->getCurrentFontName());
	applyFontStyle();

	connect(PALETTEMANAGER, SIGNAL(paletteSelected(const Palette &)), this, SLOT(onPaletteSelected(const Palette&)));
	connect(PALETTEMANAGER, SIGNAL(finished()), this, SLOT(onPaletteManagerFinished()));
}

void TextSettingsBar::reset()
{
	paletteWidget->reset();
	paletteWidget->setSelectedColorIndex(0);

	fontStyle = 0;
	applyFontStyle();

	styleButton->reset();
}

void TextSettingsBar::updateSetting(qint32 id)
{
	if (id == SHARED_COLORINDEX_SETTING)
	{
		qint32 newColorIndex = settings->getSetting(id).value<qint32>();

		paletteWidget->setSelectedColorIndex(newColorIndex);

		QColor newColor = paletteWidget->getSelectedColor();

		if (!newColor.isValid())
			return;

		settings->setSetting(TextElement::TextColor, newColor);
	}
}

void TextSettingsBar::applyFontStyle()
{
	settings->setSetting(TextElement::FontStyle, fontStyle);
}

void TextSettingsBar::onColorSelected(qint32 index)
{
	if (index == settings->getSetting(SHARED_COLORINDEX_SETTING).toInt())
		return;

	settings->setSetting(SHARED_COLORINDEX_SETTING, index);
}

void TextSettingsBar::onColorChanged(qint32 index, const QColor &newColor)
{
	PALETTEMANAGER->setPalette(paletteWidget->getPalette());

	if (index == paletteWidget->getSelectedColorIndex())
		settings->setSetting(TextElement::TextColor, newColor);
}

void TextSettingsBar::onFontNameSelected(const QString &fontName)
{
	settings->setSetting(TextElement::FontName, fontName);
}

void TextSettingsBar::onPaletteSelected(const Palette &palette)
{
	if (palette == paletteWidget->getPalette())
		return;

	paletteWidget->setPalette(palette);

	if (paletteWidget->getSelectedColorIndex() < 0)
		return;

	settings->setSetting(TextElement::TextColor, paletteWidget->getSelectedColor());
}

void TextSettingsBar::on_boldButton_clicked()
{
	fontStyle ^= TextElement::Bold;

	applyFontStyle();
}

void TextSettingsBar::on_italicButton_clicked()
{
	fontStyle ^= TextElement::Italic;

	applyFontStyle();
}

void TextSettingsBar::on_underlineButton_clicked()
{
	fontStyle ^= TextElement::Underline;

	applyFontStyle();
}

void TextSettingsBar::on_paletteManagerButton_toggled(bool checked)
{
	if (checked)
		PALETTEMANAGER->managePalette(paletteWidget->getPalette(), paletteManagerButton->mapToGlobal(QPoint(paletteManagerButton->width() - 4, 0)));
}

void TextSettingsBar::onPaletteManagerFinished()
{
	if (paletteManagerButton->isChecked())
		paletteManagerButton->toggle();
}

}