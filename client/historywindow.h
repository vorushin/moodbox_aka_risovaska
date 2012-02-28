#ifndef HISTORYWINDOW_H
#define HISTORYWINDOW_H

#include <QStringList>

#include "ui_historywindow.h"

#include "messagekey.h"
#include "messagetypemix.h"
#include "publishmessageinfo.h"
#include "tvwidget.h"
#include "uitools.h"

class QMenu;
class QAction;

namespace MoodBox
{

#define PUBLISH_UNSENT_ERROR_TITLE			QT_TRANSLATE_NOOP("MoodBox::HistoryWindow", "PublishUnsentMessagesErrorTitle")
#define PUBLISH_UNSENT_ERROR_TEXT			QT_TRANSLATE_NOOP("MoodBox::HistoryWindow", "PublishUnsentMessagesErrorText")

#define HISTORY_DELETE_ACTION_TEXT			QT_TRANSLATE_NOOP("MoodBox::HistoryWindow", "DeleteMessage")
#define HISTORY_SAVE_ACTION_TEXT			QT_TRANSLATE_NOOP("MoodBox::HistoryWindow", "SaveMessage")
#define HISTORY_COPY_ACTION_TEXT			QT_TRANSLATE_NOOP("MoodBox::HistoryWindow", "CopyMessage")
#define HISTORY_PUBLISH_ACTION_TEXT			QT_TRANSLATE_NOOP("MoodBox::HistoryWindow", "PublishMessage")

#define HISTORY_ICON_WIDTH 124
#define HISTORY_ICON_HEIGHT 82

using namespace Ui;

class HistoryItemListModel;
class PublishDialog;

class HistoryWindow : public QWidget, public MessageTypeMix, public HistoryWindowClass
{
	Q_OBJECT

public:
	HistoryWindow(QWidget *parent = 0);

	void setTvWidget(TVWidget *tv);
	void cleanup();

	virtual void updatePosition(const QWidget *topWidget);

signals:
	// History item is under the mouse cursor
	void beginOfPreview(const MessageKey &key);
	// Mouse left items
	void endOfPreview();
	// Click on item
	void clicked(const MessageKey &key);

	// Delete selected items
	void deleteMessages(const QList <MessageKey> &keys);

	// Save and copy selected item previews
	void saveMessage();
	void copyMessage();

public slots:
	void publishMessage(const MessageKey &key);

protected:
	HistoryItemListModel *itemModel;
	PublishDialog *publishDialog;

	QAction *deleteAction, *saveAsAction, *copyAction, *publishAction;
	QMenu *contextMenu;

	void createContextMenu();

	virtual void resizeEvent(QResizeEvent *event);
	virtual void contextMenuEvent(QContextMenuEvent *event);

	void showPublishDialog(const QList <PublishMessageInfo> &messages);

protected slots:
	// History item is under mouse cursor
	void onHover(const QModelIndex &index);
	void updateControlsAndActions();

	// Item activation event---whether through click or keyboard navigation
	void onActivation(const QModelIndex &index);
	void on_publishButton_clicked();

	// History autoscrolling stuff
	void onCollectionPopulated();
	void onCollectionItemAdded();

	// Delete
	void onDeleteSelection();
};

}

#endif // HISTORYWINDOW_H
