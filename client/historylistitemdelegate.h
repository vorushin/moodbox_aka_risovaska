#ifndef HISTORYLISTITEMDELEGATE_H
#define HISTORYLISTITEMDELEGATE_H

#include <QItemDelegate>
#include <QListView>

namespace MoodBox
{
// Spacing inside of list item
#define LIST_ITEM_SPACING 6
#define HISTORY_ITEM_MASK (":/MoodBox/Resources/history_item_mask.png")
#define HISTORY_SELECTED_ITEM_MASK (":/MoodBox/Resources/history_selected_item_mask.png")

// Delegate for display of the history list items
class HistoryListItemDelegate : public QItemDelegate
{
    Q_OBJECT

public:
    HistoryListItemDelegate(QListView *parent): QItemDelegate(parent), parentList(parent) {};

    virtual void paint(QPainter *painter, const QStyleOptionViewItem &option, const QModelIndex &index) const;
    virtual QSize sizeHint(const QStyleOptionViewItem &option, const QModelIndex &index) const;
	virtual QWidget *createEditor(QWidget *parent, const QStyleOptionViewItem &option, const QModelIndex &index) const;

private:
	QListView* parentList;

static QImage getHistoryItemMask();
static QImage getHistorySelectedItemMask();

};

}

#endif
