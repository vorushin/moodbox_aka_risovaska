#include "historyitemlistmodel.h"

#include <QDateTime>
#include <QPixmapCache>

#include "debug.h"
#include "messagefile.h"
#include "messagemanager.h"
#include "messageorganizer.h"
#include "xmlserializable.h"

namespace MoodBox
{

HistoryItemListModel::HistoryItemListModel(QObject *parent)
	: QAbstractListModel(parent), tv(NULL), collection(NULL)
{
}

int HistoryItemListModel::rowCount(const QModelIndex &parent) const
{
	Q_UNUSED(parent);

	if (!collection)
		return 0;

	return collection->size();
}

QVariant HistoryItemListModel::data(const QModelIndex &index, int role) const
{
	if (!index.isValid())
		return QVariant();

	Q_ASSERT_X(keyList.size() > index.row(), "HistoryItemListModel::data", "Item index is too big");
	Q_ASSERT_X(collection->contains(keyList[index.row()]), "HistoryItemListModel::data", "Item is not found in collection");

	MessageKey key = keyList[index.row()];

	if (role == Qt::DecorationRole)
	{
		return getThumbnail(key);
	}
	else if (role == Qt::UserRole)
	{
		return QVariant(tv->getPreviewFileName(key));
	}

	return QVariant();
}

MessageKey HistoryItemListModel::getMessageKey(const QModelIndex &index) const
{
	if (!index.isValid()) return MessageKey();

	Q_ASSERT_X(keyList.size() > index.row(), "HistoryItemListModel::getMessageKey", "Item index is too big");

	return keyList[index.row()];
}

PublishMessageInfo HistoryItemListModel::getPublishMessage(const QModelIndex &index) const
{
	if (!index.isValid())
		return PublishMessageInfo();

	MessageKey key = getMessageKey(index);
	
	return getPublishMessage(key);
}

PublishMessageInfo HistoryItemListModel::getPublishMessage(const MessageKey &key) const
{
	Q_ASSERT_X(collection->contains(key), "HistoryItemListModel::getPublishMessage", "Item is not found in collection");

	TVMessageInfo info = tv->getMessageInfo(key);
	qint32 messageId = key.getId();

	return PublishMessageInfo(info.sent, messageId, info.authorId, info.authorLogin, info.previewFileName);
}

void HistoryItemListModel::setTvWidget(TVWidget *tv)
{
	Q_ASSERT_X(this->tv == NULL, "HistoryItemListModel::setTvWidget", "setTvWidget was called before, we're assuming we're doing it once only");

	this->tv = tv;

	connect(tv, SIGNAL(collectionItemAdded(const MessageKey &)), this, SLOT(onCollectionItemAdded(const MessageKey &)));
	connect(tv, SIGNAL(collectionItemRemoved(const MessageKey &)), this, SLOT(onCollectionItemRemoved(const MessageKey &)));
	connect(tv, SIGNAL(collectionPopulated()), this, SLOT(onCollectionPopulated()));
	connect(tv, SIGNAL(collectionCleared()), this, SLOT(onCollectionCleared()));

	collection = tv->getMessagesCollection();
	onCollectionPopulated();
}

void HistoryItemListModel::onCollectionItemAdded(const MessageKey &key)
{
	int i = collection->keys().lastIndexOf(key);
	Q_ASSERT_X(i >= 0, "HistoryItemListModel::onCollectionItemAdded", "Item should be already inserted");

	if (i >= 0)
	{
		beginInsertRows(QModelIndex(), i, i);
		syncKeyList();
		endInsertRows();

		emit collectionItemAdded();
	}
}

void HistoryItemListModel::onCollectionItemRemoved(const MessageKey &key)
{
	int i = keyList.lastIndexOf(key);

	if (i >= 0)
	{
		beginRemoveRows(QModelIndex(), i, i);
		syncKeyList();
		endRemoveRows();
	}
}

void HistoryItemListModel::onCollectionPopulated()
{
	Q_ASSERT_X(keyList.size() == 0, "HistoryItemListModel::onCollectionPopulated", "Collection should be cleared before populating");

	if (collection->size() == 0)
	{
		emit collectionPopulated();
		return;
	}

	beginInsertRows(QModelIndex(), 0, collection->size() - 1);
	syncKeyList();
	endInsertRows();

	emit collectionPopulated();
}

void HistoryItemListModel::onCollectionCleared()
{
	if (keyList.size() == 0)
		return;

	beginRemoveRows(QModelIndex(), 0, keyList.size() - 1);
	syncKeyList();
	endRemoveRows();

	emit collectionPopulated();
}

void HistoryItemListModel::syncKeyList()
{
	keyList = collection->keys();
}

QIcon HistoryItemListModel::getThumbnail(const MessageKey &key) const
{
	QString previewFileName = tv->getPreviewFileName(key);
	Q_ASSERT_X(!previewFileName.isEmpty(), "HistoryItemListModel::getThumbnail", "Preview file name should not be empty");
	QString thumbnailFileName = MessageOrganizer::getThumbnailFileName(previewFileName);

	QString pixmapKey = QString("HistoryItemListModel:") + thumbnailFileName;
	QPixmap pixmap;

	if (QPixmapCache::find(pixmapKey, pixmap))
		return QIcon(pixmap);

	if (!pixmap.load(thumbnailFileName))
	{ // Generate thumbnail
		if (pixmap.load(previewFileName))
		{
			MESSAGEMANAGER->saveThumbnail(previewFileName, pixmap.toImage());
		}
	}

	QPixmapCache::insert(pixmapKey, pixmap);
	return QIcon(pixmap);
}

}