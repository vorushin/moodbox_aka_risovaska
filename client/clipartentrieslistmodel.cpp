#include "clipartentrieslistmodel.h"

#include "debug.h"

namespace MoodBox
{

ClipartEntriesListModel::ClipartEntriesListModel(QObject *parent)
	: QStandardItemModel(parent)
{
	initImageLoader();
}

QVariant ClipartEntriesListModel::data(const QModelIndex &index, int role) const
{
	if (!index.isValid())
		return QVariant();

	if (role == Qt::DecorationRole)
	{
		QStandardItem *item = itemFromIndex(index);
		QString fileName = item->text();
		if (!fileName.isEmpty() && item->icon().isNull() && !item->data(CLIPART_ENTRIES_INVALID).toBool())
		{
			imageLoader->loadImages(fileName, index.row());
		}
		return QStandardItemModel::data(index, role);
	}

	return QStandardItemModel::data(index, role);
}

void ClipartEntriesListModel::clear()
{
	imageLoader->clear();
	
	QStandardItemModel::clear();
}

void ClipartEntriesListModel::addItemFromImages(const ItemImages &images)
{
	QStandardItem *item = new QStandardItem(images.file);
	attachImages(item, images);
	appendRow(item);
}

void ClipartEntriesListModel::attachImages(QStandardItem *item, const ItemImages &images)
{
	item->setIcon(QPixmap::fromImage(images.icon));
	item->setData(images.preview, CLIPART_ENTRIES_PREVIEW_DATA);
}

void ClipartEntriesListModel::initImageLoader()
{
	imageLoader = new ClipartImageLoader(this);
	connect(imageLoader, SIGNAL(imagesLoaded(ItemImages, int)), this, SLOT(onImagesLoaded(ItemImages, int)));
}

void ClipartEntriesListModel::onImagesLoaded(ItemImages imageData, int row)
{
	if (row >= rowCount())
	{
		QDEBUG("ClipartEntriesListModel::onImagesLoaded: row is too big:" << row);
		return;
	}
	QStandardItem *item = this->item(row);
	if (imageData.icon.isNull())
	{
		item->setData(QVariant(true), CLIPART_ENTRIES_INVALID);
		QDEBUG("ClipartEntriesListModel::onImagesLoaded: invalid row #" << row);
	}
	else
	{
		item->setIcon(QPixmap::fromImage(imageData.icon));
		item->setData(QPixmap::fromImage(imageData.preview), CLIPART_ENTRIES_PREVIEW_DATA);
	}
}

}
