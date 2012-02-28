#ifndef CLIPARTENTRIESLISTMODEL_H
#define CLIPARTENTRIESLISTMODEL_H

#include <QStandardItemModel>

#include "clipartimageloader.h"

namespace MoodBox
{

// Preview data
#define CLIPART_ENTRIES_PREVIEW_DATA	QStandardItem::UserType + 1
// Used to mark items with invalid image data to delete them later
#define CLIPART_ENTRIES_INVALID			QStandardItem::UserType + 2

class ClipartEntriesListModel : public QStandardItemModel
{
	Q_OBJECT

public:
	ClipartEntriesListModel(QObject *parent);

	virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;

	void clear();

	void addItemFromImages(const ItemImages &images);

protected:
	ClipartImageLoader *imageLoader;

	void attachImages(QStandardItem *item, const ItemImages &images);
	void initImageLoader();

protected slots:
	void onImagesLoaded(ItemImages imageData, int row);
};

}

#endif // CLIPARTENTRIESLISTMODEL_H
