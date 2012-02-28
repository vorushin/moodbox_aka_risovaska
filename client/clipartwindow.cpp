#include "clipartwindow.h"

#include <QDir>
#include <QDesktopServices>
#include <QFileDialog>
#include <QListWidgetItem>
#include <QMouseEvent>
#include <QSettings>
#include <QSizeGrip>
#include <QStringList>
#include <QToolButton>

#include "apptools.h"
#include "clipartimageloader.h"
#include "palettemanager.h"

#include "svgtool.h"

#include "testtools.h"

namespace MoodBox
{

ClipartWindow::ClipartWindow(QWidget *parent)
	: MovableWidget(parent, Qt::FramelessWindowHint | Qt::Tool)
{
	TimeMeasure t("ClipartWindow");

	setupUi(this);
	
	t.showTimePassedAfterSetupUi();

	isShowFirstTime = true;

	connect(closeToolButton, SIGNAL(clicked()), this, SLOT(close()));

	// Clipart tab
	connect(clipartCategorySelector, SIGNAL(clipartLoadRequest(const QList<ItemImages> &)), 
		clipartEntriesList, SLOT(loadEntries(const QList<ItemImages> &)));
	clipartCategorySelector->loadCategories();

	// Photo tab
	QString photoFolder = QSettings().value(CLIPART_LAST_PHOTO_FOLDER_OPTION).toString();
	if (photoFolder.isEmpty())
	{
		photoFolder = QDesktopServices::storageLocation(QDesktopServices::PicturesLocation);
		if (photoFolder.isEmpty())
			photoFolder = QDir::homePath();
	}
	
	setPhotoFolder(photoFolder);

	// Backgrounds tab
	fillBackgrounds(PALETTEMANAGER->getPalette());
	connect(PALETTEMANAGER, SIGNAL(paletteSelected(const Palette &)), this, SLOT(fillBackgrounds(const Palette &)));

	new QSizeGrip(widget);

	// Default SVG size
	Velasquez::SvgTool::setDefaultSize(QSizeF(PICTURE_PREVIEW_WIDTH, PICTURE_PREVIEW_HEIGHT));

	tabWidget->setCurrentIndex(DEFAULT_CLIPART_TAB);
}

void ClipartWindow::showPage(ClipartPage::ClipartPageEnum page)
{
	int num;

	switch (page)
	{
		case ClipartPage::Photos: num = 1; break;
		case ClipartPage::Background:  num = 2; break;
			
		default: num = 0;
	}

	tabWidget->setCurrentIndex(num);

	if (!isVisible())
		show();
}

void ClipartWindow::resizeEvent(QResizeEvent *event)
{
	setMask(WidgetMaskCreator::createMask(WidgetMaskCreator::WidgetMaskSmall, width(), height()));
	MovableWidget::resizeEvent(event);
}

void ClipartWindow::fillBackgrounds(const Palette &palette)
{
	QList<ItemImages> images;

	// POSTPONED add background color info when generating images to avoid later extraction from pixmap
	for (int i = 0; i < PALETTE_COLOR_COUNT; i++)
	{
		int iconSize = ClipartImageLoader::getMaxIconSize();
		QImage image(iconSize, iconSize, QImage::Format_ARGB32);
		image.fill(palette.getColor(i).rgba());
		images.append(ClipartImageLoader::generateItemImages(QString(), image));
	}

	tileList->setImageEntries(images);
}

void ClipartWindow::setPhotoFolder(const QString &folderName)
{
	photoFolder = folderName;

	folderLabel->setText(QDir::toNativeSeparators(photoFolder));
	photoList->loadFolder(photoFolder);
}

void ClipartWindow::on_browseButton_clicked()
{
	QString selectedFolder = QFileDialog::getExistingDirectory(this, tr(CLIPART_SELECT_FOLDER_DIALOG_TITLE), photoFolder);
	
	if (selectedFolder.isEmpty()) 
		return;

	QSettings().setValue(CLIPART_LAST_PHOTO_FOLDER_OPTION, selectedFolder);

	setPhotoFolder(selectedFolder);
}

}