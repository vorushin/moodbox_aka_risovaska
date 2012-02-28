#ifndef CLIPARTENTRIESLISTITEMDELEGATE_H
#define CLIPARTENTRIESLISTITEMDELEGATE_H

#include <QItemDelegate>
#include <QListView>

namespace MoodBox
{
// Spacing inside of list item, in percents
#define LIST_ITEM_SPACING 10

// Delegate for display of the clipart list entry
class ClipartEntriesListItemDelegate : public QItemDelegate
{
    Q_OBJECT

public:
    ClipartEntriesListItemDelegate(QListView *parent): QItemDelegate(parent), parentList(parent) {};

    virtual void paint(QPainter *painter, const QStyleOptionViewItem &option, const QModelIndex &index) const;
    virtual QSize sizeHint(const QStyleOptionViewItem &option, const QModelIndex &index) const;
	virtual QWidget *createEditor(QWidget *parent, const QStyleOptionViewItem &option, const QModelIndex &index) const;

private:
	QListView* parentList;
};

}

#endif
