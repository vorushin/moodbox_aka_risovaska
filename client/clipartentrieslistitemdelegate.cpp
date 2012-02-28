#include "clipartentrieslistitemdelegate.h"

#include <QPainter>

namespace MoodBox
{

void ClipartEntriesListItemDelegate::paint(QPainter *painter, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
	QIcon imageIcon = index.data(Qt::DecorationRole).value<QIcon>();

	// Size without spacing
	int spacingWidth = option.rect.width() * LIST_ITEM_SPACING / 100;
	int spacingHeight = option.rect.height() * LIST_ITEM_SPACING / 100;
	QRect tightRect = option.rect.adjusted(spacingWidth, spacingHeight, -spacingWidth, -spacingHeight);

	if ((option.state & QStyle::State_MouseOver) && 
		parentList->rect().contains(parentList->mapFromGlobal(QCursor::pos()))) // Necessary to remove hover after drop
	{
		painter->fillRect(option.rect, QColor(240, 240, 240));
	}
	else if ((option.state & QStyle::State_Selected) && parentList->isSelectionRectVisible())
		painter->fillRect(option.rect, option.palette.highlight());
	else
		painter->fillRect(option.rect, Qt::white);

	imageIcon.paint(painter, tightRect);

	painter->setPen(Qt::lightGray);
	painter->drawRect(option.rect.adjusted(0, 0, -1, -1));
}

QSize ClipartEntriesListItemDelegate::sizeHint(const QStyleOptionViewItem &option, const QModelIndex &index) const
{
	Q_UNUSED(option)
	Q_UNUSED(index)

	Q_ASSERT_X(parentList->iconSize().width() > 0 && parentList->iconSize().height() > 0, 
		"ClipartEntriesListItemDelegate::sizeHint", "Icon size can't be zero or auto");
	
	QSize iconSize = parentList->iconSize();

	return iconSize;
}

QWidget *ClipartEntriesListItemDelegate::createEditor(QWidget *parent, const QStyleOptionViewItem &option, const QModelIndex &index) const  
{
	Q_UNUSED(parent)
	Q_UNUSED(option)
	Q_UNUSED(index)

	return NULL;
}

}