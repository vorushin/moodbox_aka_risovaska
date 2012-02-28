#include "palettemanager.h"

PaletteManager::PaletteManager(QWidget *parent)
	: QDialog(parent)
{
	setupUi(this);

	currentPalette->setDirection(PaletteWidget::Horizonal);
	currentPalette->setReadOnly(true);

	standardPalettes->setFileName("standard.pal");
	standardPalettes->setPalettesRemovable(false);
	connect(standardPalettes, SIGNAL(paletteSelected(const Palette &)), currentPalette, SLOT(setPalette(const Palette &)));

	customPalettes->setFileName("custom.pal");
	connect(customPalettes, SIGNAL(paletteSelected(const Palette &)), currentPalette, SLOT(setPalette(const Palette &)));
	connect(customPalettes, SIGNAL(addNewPalette()), this, SLOT(onAddPalette()));

	connect(selectButton, SIGNAL(pressed()), this, SLOT(accept()));
}

PaletteManager::~PaletteManager()
{
}

void PaletteManager::setPalette(const Palette &palette)
{
	currentPalette->setPalette(palette);
}

Palette PaletteManager::getPalette() const
{
	return currentPalette->getPalette();
}

void PaletteManager::onAddPalette()
{
	customPalettes->addPalette(currentPalette->getPalette());
}
