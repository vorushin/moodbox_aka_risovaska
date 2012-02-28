#include "clipartentrieslist.h"

#include <QApplication>
#include <QDir>
#include <QDrag>
#include <QMimeData>
#include <QMouseEvent>
#include <QScrollBar>
#include <QTimer>
#include <QUrl>

#include "apptools.h"
#include "clipartentrieslistitemdelegate.h"
#include "clipartentrieslistmodel.h"
#include "vcommon.h"
#include "testtools.h"

namespace MoodBox
{

ClipartEntriesList::ClipartEntriesList(QWidget *parent)
	: QListView(parent)
{
	TimeMeasure t("ClipartEntriesList");

	itemModel = new ClipartEntriesListModel(this);
	itemModel->setColumnCount(1);
	setModel(itemModel);

	setItemDelegate(new ClipartEntriesListItemDelegate(this));
	setSpacing(0); // Necessary for icon size calculation to work ok

	setSelectionRectVisible(false); // We don't need to show selection in clipart

	// Looking for top-most widget to avoid preview clipping
	if (parent != NULL)
		for(; parent->parentWidget() != NULL; parent = parent->parentWidget());
	// Setting up preview
	previewLabel = new QLabel(parent);
	previewLabel->setAlignment(Qt::AlignCenter);
	previewLabel->hide();
}

void ClipartEntriesList::setImageEntries(const QList<ItemImages> &images)
{
	clearModel();

	// Filling in the values
	foreach(ItemImages image, images)
	{
		itemModel->addItemFromImages(image);
	}
}

void ClipartEntriesList::loadFolder(const QString &folderName)
{
	QDir clipartFolder(folderName);

	if (!clipartFolder.exists())
		return;

	clearModel();

	QStringList entries = clipartFolder.entryList(QString(SUPPORTED_IMAGE_FORMATS).split(" ", QString::SkipEmptyParts), 
		QDir::Files | QDir::Readable);

	for (int i = 0; i < entries.size(); i++)
	{
		QString fileName(AppTools::addPathSeparator(folderName) + entries[i]);
		itemModel->appendRow(new QStandardItem(fileName));
	}
}

void ClipartEntriesList::loadEntries(const QList<ItemImages> &entries)
{
	clearModel();
	foreach (ItemImages entry, entries)
	{
		itemModel->addItemFromImages(entry);
	}
}

void ClipartEntriesList::resizeEvent(QResizeEvent *event)
{
	int columnWidth = (viewport()->width() - verticalScrollBar()->width() - LIST_WIDTH_SUBTRACT) / CLIPART_COLUMNS;
	setIconSize(QSize(columnWidth, columnWidth));
	QListView::resizeEvent(event);
}

void ClipartEntriesList::startDrag(Qt::DropActions supportedActions)
{
	QPixmap draggingPixmap = getCurrentPixmap();

	if (draggingPixmap.isNull())
		return;

	QMimeData *data = new QMimeData;

	// Set text (it's used to detect whether we want to fill background or just add a picture)
	QString text = currentIndex().data().toString();

	// Not empty text means real image, empty - background
	if (!text.isEmpty())
	{
		data->setText(text);

		QList <QUrl> urls;
		urls.append(QUrl::fromLocalFile(text));
		data->setUrls(urls);
	}
	else
	{
		data->setData(BACKGROUND_COLOR_TYPE, getBackgroundData(draggingPixmap));
	}

    // Drag initialization
	QDrag *drag = new QDrag(this);

	drag->setPixmap(draggingPixmap);
    drag->setMimeData(data);
	drag->setHotSpot(QPoint(draggingPixmap.width() / 2, draggingPixmap.height() / 2));

    // Let's go!
	drag->start(supportedActions);
}

void ClipartEntriesList::mousePressEvent(QMouseEvent *event)
{
	QListView::mousePressEvent(event);

	turnPreview(true, event->globalPos());
	previewStartPos = event->pos();
}

void ClipartEntriesList::mouseMoveEvent(QMouseEvent *event)
{
	if (event->buttons() & Qt::LeftButton)
	{
		int distance = (event->pos() - previewStartPos).manhattanLength();
		
		// Show preview till drag starts
		if (distance > QApplication::startDragDistance())
			turnPreview(false);
		else
			turnPreview(true, event->globalPos());
	}

	QListView::mouseMoveEvent(event);
}

void ClipartEntriesList::mouseReleaseEvent(QMouseEvent *event)
{
	turnPreview(false);

	QListView::mouseReleaseEvent(event);
}

void ClipartEntriesList::clearModel()
{
	itemModel->clear();
}

void ClipartEntriesList::turnPreview(bool on, QPoint mousePosition)
{
	// Exiting if element isn't clicked
	if (on && !indexAt(mapFromGlobal(mousePosition)).isValid())
		return;

	bool changed = (on != previewLabel->isVisible());

	previewLabel->setVisible(on);

	if (on && changed)
		QApplication::setOverrideCursor(Qt::BlankCursor);
	else
		QApplication::restoreOverrideCursor();

	// Nothing to do if progress pane is disabled
	if (!previewLabel->isVisible())
		return;

	// Need to load image first time
	if (changed)
	{
		QPixmap preview = getCurrentPixmap();
		previewLabel->resize(preview.size());
		previewLabel->setPixmap(preview);
	}

	mousePosition -= QPoint(previewLabel->width() / 2, previewLabel->height() / 2);
	previewLabel->move(previewLabel->parentWidget()->mapFromGlobal(mousePosition));
}

QPixmap ClipartEntriesList::getCurrentPixmap(int pixmapType) const
{
	QPixmap pixmap;

	if (currentIndex().isValid())
		pixmap = currentIndex().data(pixmapType).value<QPixmap>();

	return pixmap;
}

QByteArray ClipartEntriesList::getBackgroundData(const QPixmap &pixmap) const
{
	QRgb color = pixmap.toImage().pixel(0, 0);

	QByteArray data;
	QDataStream dataStream(&data, QIODevice::WriteOnly);

	dataStream << QColor(color);

	return data;
}

}