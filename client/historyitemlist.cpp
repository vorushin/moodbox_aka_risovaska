#include "historyitemlist.h"

#include <QApplication>
#include <QKeyEvent>
#include <QScrollBar>

#include "debug.h"
#include "draggablelabel.h"

namespace MoodBox
{

HistoryItemList::HistoryItemList(QWidget *parent)
	: QListView(parent)
{
	previewLabel = new DraggableLabel(this);
#ifdef Q_WS_X11    
    previewLabel->setGeometry(0, 0, 0, 0);
#else
    previewLabel->hide();
#endif    
    
#ifdef Q_WS_MAC    
    setAttribute(Qt::WA_MacShowFocusRect, false);        
#endif
}

void HistoryItemList::leaveEvent(QEvent *event)
{
	QListView::leaveEvent(event);

	emit mouseLeft();
}

void HistoryItemList::keyPressEvent(QKeyEvent *event)
{
	int key = -1;

	if (event->key() == Qt::Key_PageUp)
	{
		key = Qt::Key_Up;
	}
	else if (event->key() == Qt::Key_PageDown)
	{
		key = Qt::Key_Down;
	}

	if (key >= 0)
	{
		event->accept();
		for (int i = 0; i < HISTORY_PAGE_SCROLL_ITEMS; i++)
		{
			QCoreApplication::postEvent(this, new QKeyEvent(event->type(), key, event->modifiers(), event->text()));
		}
	}
	else
	{
		QListView::keyPressEvent(event);
	}
}

QItemSelectionModel::SelectionFlags HistoryItemList::selectionCommand(const QModelIndex &index, const QEvent *event) const
{
	if (event && event->type() == QEvent::MouseMove)
	{
		return QItemSelectionModel::NoUpdate;
	}

	return QListView::selectionCommand(index, event);
}

void HistoryItemList::mouseMoveEvent(QMouseEvent *event)
{
	QAbstractItemView::mouseMoveEvent(event);
	setState(NoState);
	QCoreApplication::postEvent(previewLabel, new QMouseEvent(event->type(), event->pos(), event->button(), event->buttons(), event->modifiers()));
}

void HistoryItemList::mousePressEvent(QMouseEvent *event)
{
	QListView::mousePressEvent(event);

	QModelIndex index = indexAt(event->pos());
	if (!index.isValid()) return;

	previewLabel->setPixmap(QPixmap(index.data(Qt::UserRole).toString()));
	QCoreApplication::postEvent(previewLabel, new QMouseEvent(event->type(), event->pos(), event->button(), event->buttons(), event->modifiers()));
}

void HistoryItemList::selectionChanged(const QItemSelection &selected, const QItemSelection &deselected)
{
	QListView::selectionChanged(selected, deselected);

	emit selectionChanged();
}

void HistoryItemList::currentChanged(const QModelIndex &current, const QModelIndex &previous)
{
	QListView::currentChanged(current, previous);

	if (previous.isValid())
		emit itemActivated(current);
}

void HistoryItemList::updateGeometries()
{
	QListView::updateGeometries();
	verticalScrollBar()->setSingleStep(verticalScrollBar()->singleStep() / QApplication::wheelScrollLines());
}

}