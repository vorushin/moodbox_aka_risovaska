#ifndef HISTORYITEMLIST_H
#define HISTORYITEMLIST_H

#include <QListView>

namespace MoodBox
{

class DraggableLabel;

#define HISTORY_PAGE_SCROLL_ITEMS	3

// List of history items in HistoryWindow
class HistoryItemList : public QListView
{
	Q_OBJECT

public:
	HistoryItemList(QWidget *parent = 0);

signals:
	// Mouse left the list
	void mouseLeft();
	void selectionChanged();
	void itemActivated(const QModelIndex &index);

protected:
	DraggableLabel *previewLabel;

	virtual void leaveEvent(QEvent *event);
	// Override PgUp/PgDown behavior
	virtual void keyPressEvent(QKeyEvent *event);
	// Overriden to prohibit item selection with mouse
	virtual QItemSelectionModel::SelectionFlags selectionCommand(const QModelIndex &index, const QEvent *event = 0) const;
	// Don't display selection rectangle while dragging mouse
	virtual void mouseMoveEvent(QMouseEvent *event);
	// Remember drag position
	virtual void mousePressEvent(QMouseEvent *event);

protected slots:
	virtual void selectionChanged(const QItemSelection &selected, const QItemSelection &deselected);
	virtual void currentChanged(const QModelIndex &current, const QModelIndex &previous);
	// Restore scrollbar single step here
	virtual void updateGeometries();
};

}

#endif // HISTORYITEMLIST_H
