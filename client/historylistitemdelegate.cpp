#include "historylistitemdelegate.h"

#include <QPainter>

namespace MoodBox
{

void HistoryListItemDelegate::paint(QPainter *painter, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
	QIcon imageIcon = index.data(Qt::DecorationRole).value<QIcon>();

	// Size without spacing
	QRect newRect = option.rect;
	QRect tightRect = newRect.adjusted(LIST_ITEM_SPACING, LIST_ITEM_SPACING, -LIST_ITEM_SPACING, -LIST_ITEM_SPACING);
	
	imageIcon.paint(painter, tightRect);

	if ((option.state & QStyle::State_Selected) && parentList->isSelectionRectVisible())
		painter->drawImage(newRect.left() + 1, newRect.top() + 1, getHistorySelectedItemMask());
	else
		painter->drawImage(newRect.left() + 1, newRect.top() + 2, getHistoryItemMask());
}

QSize HistoryListItemDelegate::sizeHint(const QStyleOptionViewItem &option, const QModelIndex &index) const
{
	Q_UNUSED(option)
	Q_UNUSED(index)

	Q_ASSERT_X(parentList->iconSize().width() > 0 && parentList->iconSize().height() > 0, 
		"HistoryListItemDelegate::sizeHint", "Icon size can't be zero or auto");
	
	QSize iconSize = parentList->iconSize();

	return iconSize;
}

QImage HistoryListItemDelegate::getHistoryItemMask()
{
	static QImage historyItemMask(HISTORY_ITEM_MASK);

	return historyItemMask;
}

QImage HistoryListItemDelegate::getHistorySelectedItemMask()
{
	static QImage historySelectedItemMask(HISTORY_SELECTED_ITEM_MASK);

	return historySelectedItemMask;
}

QWidget *HistoryListItemDelegate::createEditor(QWidget *parent, const QStyleOptionViewItem &option, const QModelIndex &index) const  
{
	Q_UNUSED(parent)
	Q_UNUSED(option)
	Q_UNUSED(index)

	return NULL;
}

}