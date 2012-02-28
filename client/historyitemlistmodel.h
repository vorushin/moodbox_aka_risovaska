#ifndef HISTORYITEMLISTMODEL_H
#define HISTORYITEMLISTMODEL_H

#include <QAbstractListModel>

#include "messagekey.h"
#include "publishmessageinfo.h"
#include "tvwidget.h"

namespace MoodBox
{

class HistoryItemListModel : public QAbstractListModel
{
	Q_OBJECT

public:
	HistoryItemListModel(QObject *parent = 0);

	virtual int rowCount(const QModelIndex &parent = QModelIndex()) const;
	virtual QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const;

	MessageKey getMessageKey(const QModelIndex &index) const;
	PublishMessageInfo getPublishMessage(const QModelIndex &index) const;
	PublishMessageInfo getPublishMessage(const MessageKey &key) const;

	void setTvWidget(TVWidget *tv);

signals:
	void collectionPopulated();
	void collectionItemAdded();

public slots:
	void onCollectionItemAdded(const MessageKey &key);
	void onCollectionItemRemoved(const MessageKey &key);
	void onCollectionPopulated();
	void onCollectionCleared();

protected:
	TVWidget *tv;

//	const QMap <MessageKey, TVMessageInfo> *collection; // Collection of messages
	const QMap <MessageKey, int> *collection; // Collection of messages
	QList<MessageKey> keyList;

	void syncKeyList();

	QIcon getThumbnail(const MessageKey &key) const;
};

}

#endif // HISTORYITEMLISTMODEL_H
