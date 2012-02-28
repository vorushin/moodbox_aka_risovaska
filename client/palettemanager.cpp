#include "palettemanager.h"

#include <QDesktopWidget>
#include <QShortcut>

#include "peopleinfomanager.h"
#include "apptools.h"
#include "uitools.h"

#include "debug.h"
#include "testtools.h"

namespace MoodBox
{

PaletteManager *PaletteManager::commonPaletteManager = NULL;

PaletteManager::PaletteManager(QWidget *parent)
	: QWidget(parent, Qt::Popup)
{
	TimeMeasure t("PaletteManager");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	standardPaletteTab->layout()->setMargin(1);
	customPaletteTab->layout()->setMargin(1);

	// Setup standard and default palettes
	standardPalettes->setPalettesRemovable(false);
	QString standardName = AppTools::getResourcesFolder()+QString(STANDARD_PALETTES_FILENAME);
	standardPalettes->setFileName(standardName);

	if (standardPalettes->getPaletteCount() > 0)
		defaultPalette = standardPalettes->getPalette(0);

	currentPalette->setPalette(defaultPalette);

	connect(standardPalettes, SIGNAL(paletteSelected(const Palette &)), this, SLOT(onPaletteSelected(const Palette &)));

	connect(customPalettes, SIGNAL(paletteSelected(const Palette &)), this, SLOT(onPaletteSelected(const Palette &)));

	connect(closeToolButton, SIGNAL(pressed()), this, SLOT(close()));

	// show help by pressing F1
	QShortcut * showHelpShortCut = new QShortcut(this);
	showHelpShortCut->setKey(Qt::Key_F1);
	connect(showHelpShortCut, SIGNAL(activated()), this, SLOT(showHelp()));
}

void PaletteManager::setPalette(const Palette &palette)
{
	if (currentPalette->getPalette() == palette)
		return;

	currentPalette->setPalette(palette);
	emit paletteSelected(palette);
}

Palette PaletteManager::getPalette() const
{
	return currentPalette->getPalette();
}

void PaletteManager::managePalette(const Palette &palette, const QPoint &showLocation)
{
	setPalette(palette);
	
	if (!showLocation.isNull())
	{
		QRect availableGeometry = QApplication::desktop()->availableGeometry(this);

		int x = showLocation.x();

		if (x + width() > availableGeometry.right())
			x = availableGeometry.right() - width();

		int y = showLocation.y();

		if (y + height() > availableGeometry.bottom())
			y = availableGeometry.bottom() - height();

		move(x, y);
	}

	show();
}

PaletteManager *PaletteManager::getCommonManager()
{
	if (commonPaletteManager == NULL)
		commonPaletteManager = new PaletteManager();

	return commonPaletteManager;
}

void PaletteManager::showEvent(QShowEvent *event)
{
	QString customFolder = AppTools::addPathSeparator(INFOMANAGER->getUserSettingsFolder());
	customPalettes->setFileName(customFolder + CUSTOM_PALETTES_FILENAME);

	QWidget::showEvent(event);
}

void PaletteManager::resizeEvent(QResizeEvent *event)
{
	setMask(WidgetMaskCreator::createMask(WidgetMaskCreator::WidgetMaskSmall, width(), height()));
	QWidget::resizeEvent(event);
}

void PaletteManager::closeEvent(QCloseEvent *event)
{
	QWidget::closeEvent(event);

	emit finished();
}

void PaletteManager::onPaletteSelected(const Palette &palette)
{
	currentPalette->setPalette(palette);
	emit paletteSelected(palette);

	close();
}

void PaletteManager::on_savePaletteButton_clicked()
{
	Palette palette = currentPalette->getPalette();

	customPalettes->addPalette(palette);
	paletteTabWidget->setCurrentIndex(1);
}

void PaletteManager::showHelp()
{
	UiTools::showHelp();
}

}