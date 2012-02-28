#include "historywindow.h"

#include <QDir>
#include <QMessageBox>
#include <QScrollBar>
#include <QTimer>
#include <QMenu>
#include <QAction>

#include "historylistitemdelegate.h"
#include "debug.h"
#include "historyitemlistmodel.h"
#include "messagefile.h"
#include "messageorganizer.h"
#include "peopleinfomanager.h"
#include "publishdialog.h"
#include "testtools.h"
#include "uitools.h"
#include "vcommon.h"

#ifdef Q_WS_MAC
#include "mactools.h"
#endif
#ifdef Q_WS_X11
#include "linuxtools.h"
#endif

namespace MoodBox
{

HistoryWindow::HistoryWindow(QWidget *parent)
	: QWidget(parent, Qt::FramelessWindowHint | Qt::Tool), publishDialog(NULL)
{
#ifdef Q_WS_MAC
    MacTools::addWindow(this);
#endif
#ifdef Q_WS_X11
    LinuxTools::addWindow(this);
#endif

	TimeMeasure t("HistoryWindow");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	itemModel = new HistoryItemListModel(this);
	historyItemsList->setModel(itemModel);
	historyItemsList->setItemDelegate(new HistoryListItemDelegate(historyItemsList));
	
	historyItemsList->setMouseTracking(true);

	historyItemsList->setIconSize(QSize(HISTORY_ICON_WIDTH, HISTORY_ICON_HEIGHT));

	publishButton->setEnabled(false);

	// Contect menu
	createContextMenu();

	// Hover tracking
	connect(historyItemsList, SIGNAL(entered(const QModelIndex &)), this, SLOT(onHover(const QModelIndex &)));
	connect(historyItemsList, SIGNAL(mouseLeft()), this, SIGNAL(endOfPreview()));
	// Item selection
	connect(historyItemsList, SIGNAL(selectionChanged()), this, SLOT(updateControlsAndActions()));
	// Activation event via mouse click or keyboard navigation
	connect(historyItemsList, SIGNAL(clicked(const QModelIndex &)), this, SLOT(onActivation(const QModelIndex &)));
	connect(historyItemsList, SIGNAL(itemActivated(const QModelIndex &)), this, SLOT(onActivation(const QModelIndex &)));

	// History autoscrolling
	connect(itemModel, SIGNAL(collectionPopulated()), this, SLOT(onCollectionPopulated()));
	connect(itemModel, SIGNAL(collectionItemAdded()), this, SLOT(onCollectionItemAdded()));

	// Online state
	connect(INFOMANAGER, SIGNAL(isOnlineChanged()), this, SLOT(updateControlsAndActions()));
}

void HistoryWindow::setTvWidget(TVWidget *tv)
{
	itemModel->setTvWidget(tv);
	historyItemsList->scrollToBottom();
}

void HistoryWindow::cleanup()
{
	if (publishDialog != NULL)
		publishDialog->cleanup();
}

void HistoryWindow::updatePosition(const QWidget *topWidget)
{
	setGeometry(topWidget->geometry().x() + topWidget->geometry().width() - 12/*TODO*/, 
		topWidget->geometry().y(), width(), topWidget->height());
}

void HistoryWindow::createContextMenu()
{
	contextMenu = new QMenu(this);

	deleteAction = new QAction(tr(HISTORY_DELETE_ACTION_TEXT), this);
	connect(deleteAction, SIGNAL(triggered()), this, SLOT(onDeleteSelection()));
	contextMenu->addAction(deleteAction);

	saveAsAction = new QAction(tr(HISTORY_SAVE_ACTION_TEXT), this);
	connect(saveAsAction, SIGNAL(triggered()), this, SIGNAL(saveMessage()));
	contextMenu->addAction(saveAsAction);

	copyAction = new QAction(tr(HISTORY_COPY_ACTION_TEXT), this);
	connect(copyAction, SIGNAL(triggered()), this, SIGNAL(copyMessage()));
	contextMenu->addAction(copyAction);

	publishAction = new QAction(tr(HISTORY_PUBLISH_ACTION_TEXT), this);
	connect(publishAction, SIGNAL(triggered()), this, SLOT(on_publishButton_clicked()));
	contextMenu->addAction(publishAction);	
}

void HistoryWindow::publishMessage(const MessageKey &key)
{
	QList <PublishMessageInfo> messages;

	messages.append(itemModel->getPublishMessage(key));

	showPublishDialog(messages);
}

void HistoryWindow::resizeEvent(QResizeEvent *event)
{
	setMask(WidgetMaskCreator::createMask(WidgetMaskCreator::WidgetMaskHistory, width(), height()));
	
	QWidget::resizeEvent(event);
}

void HistoryWindow::contextMenuEvent(QContextMenuEvent *event)
{
	contextMenu->exec(event->globalPos());

	QWidget::contextMenuEvent(event);
}

void HistoryWindow::showPublishDialog(const QList <PublishMessageInfo> &messages)
{
	if (publishDialog == NULL)
		publishDialog = new PublishDialog(NULL);

	publishDialog->setMessages(messages);
	publishDialog->setRecipientId(getRecipient());
	publishDialog->show();
}

void HistoryWindow::onHover(const QModelIndex &index)
{
	emit beginOfPreview(itemModel->getMessageKey(index));
}

void HistoryWindow::updateControlsAndActions()
{
	QItemSelectionModel *selection = historyItemsList->selectionModel();

	bool deleteEnabled = false;
	bool saveCopyEnabled = false;
	bool publishEnabled = false;

	if (selection->hasSelection())
	{
		const QString previewFile = itemModel->data(selection->currentIndex(), Qt::UserRole).toString();
		const bool hasPreview = QFile::exists(previewFile);	

		QModelIndexList indexes = selection->selectedIndexes();

		deleteEnabled = true;

		if (indexes.count() == 1)
			saveCopyEnabled = hasPreview;
		else
			saveCopyEnabled = false;
		 
		publishEnabled = INFOMANAGER->isUserOnline() && hasPreview;
	}

	publishButton->setEnabled(publishEnabled);

	deleteAction->setEnabled(deleteEnabled);
	saveAsAction->setEnabled(saveCopyEnabled);
	copyAction->setEnabled(saveCopyEnabled);
	publishAction->setEnabled(publishButton->isEnabled());
}

void HistoryWindow::onActivation(const QModelIndex &index)
{
	emit clicked(itemModel->getMessageKey(index));
}

void HistoryWindow::on_publishButton_clicked()
{
	QItemSelectionModel *selection = historyItemsList->selectionModel();
	if (!selection->hasSelection())
		return;

	QModelIndexList indexes = selection->selectedIndexes();

	// Form messages for publishing
	QMap<MessageKey,PublishMessageInfo> messages;
	foreach (QModelIndex index, indexes)
	{
		PublishMessageInfo info = itemModel->getPublishMessage(index);
		if(!info.isSent)
		{
			QMessageBox::warning(this, tr(PUBLISH_UNSENT_ERROR_TITLE), tr(PUBLISH_UNSENT_ERROR_TEXT));
			return;
		}

		messages.insert(itemModel->getMessageKey(index), info);
	}

	// Do publishing, hooray!
	showPublishDialog(messages.values());
}

void HistoryWindow::onCollectionPopulated()
{
	historyItemsList->clearSelection();
	historyItemsList->scrollToBottom();
}

void HistoryWindow::onCollectionItemAdded()
{
	// Get the item at the bottom center of the list
	QModelIndex index = historyItemsList->indexAt(QPoint(historyItemsList->width() / 2, historyItemsList->height() - historyItemsList->spacing() - 1));
	if (!index.isValid()) return;

	// Scroll to bottom if next to last item is at the bottom
	if (index.row() == itemModel->rowCount() - 2)
		onCollectionPopulated();
}

void HistoryWindow::onDeleteSelection()
{
	QItemSelectionModel *selection = historyItemsList->selectionModel();
	QModelIndexList indexes = selection->selectedIndexes();

	QList <MessageKey> deleteList;

	foreach (QModelIndex index, indexes)
	{
		deleteList << itemModel->getMessageKey(index);
	}

	emit deleteMessages(deleteList);
}

}
