#include "brushstylewidget.h"

#include <QStandardItem>
#include <QPainter>

#include "testtools.h"

namespace MoodBox
{

BrushStyleWidget::BrushStyleWidget(QWidget *parent)
	: QWidget(parent, Qt::Popup), style(BrushStyle::Size)
{
	TimeMeasure t("BrushStyleWidget");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	itemModel = new QStandardItemModel(this);
	itemModel->setColumnCount(1);
	itemDelegate = new BrushStyleListItemDelegate(this);

	styleList->setMouseTracking(true);
	styleList->setItemDelegate(itemDelegate);
	styleList->setModel(itemModel);

	currentSizeIndex = revertIndex(DEFAULT_BRUSH_SIZE_INDEX);
	currentAlphaIndex = revertIndex(DEFAULT_BRUSH_ALPHA_INDEX);

	buildStyleList();

	connect(styleList, SIGNAL(clicked(const QModelIndex &)), this, SLOT(onItemSelected(const QModelIndex &)));
	connect(styleList, SIGNAL(entered(const QModelIndex &)), this, SLOT(onItemHovered(const QModelIndex &)));
}

void BrushStyleWidget::setStyle(BrushStyle::BrushStyleEnum style)
{
	if (this->style == style)
		return;

	this->style = style;
	buildStyleList();
}

BrushStyle::BrushStyleEnum BrushStyleWidget::getStyle() const
{
	return this->style;
}

void BrushStyleWidget::setSizeIndex(int sizeIndex)
{
	if (getSizeIndex() == sizeIndex || sizeIndex < 0 || sizeIndex >= BRUSH_SIZE_COUNT)
		return;

	sizeIndex = revertIndex(sizeIndex);
	currentSizeIndex = sizeIndex;

	if (getStyle() == BrushStyle::Alpha)
	{
		styleList->update();
	}
	else
	{
		styleList->setCurrentIndex(itemModel->index(sizeIndex, 0));
	}
}

int BrushStyleWidget::getSizeIndex() const
{
	return revertIndex(currentSizeIndex);
}

void BrushStyleWidget::setAlphaIndex(int alphaIndex)
{
	if (getAlphaIndex() == alphaIndex || alphaIndex < 0 || alphaIndex >= BRUSH_ALPHA_COUNT)
		return;

	alphaIndex = revertIndex(alphaIndex);
	currentAlphaIndex = alphaIndex;

	if (getStyle() == BrushStyle::Alpha)
	{	
		styleList->setCurrentIndex(itemModel->index(alphaIndex, 0));
	}
}

int BrushStyleWidget::getAlphaIndex() const
{
	return revertIndex(currentAlphaIndex);
}

void BrushStyleWidget::setColor(const QColor &color)
{
	if (currentColor == color)
		return;

	currentColor = color;

	if (getStyle() == BrushStyle::Size)
		styleList->update();
}

void BrushStyleWidget::reset()
{
	setSizeIndex(DEFAULT_BRUSH_SIZE_INDEX);
	setAlphaIndex(DEFAULT_BRUSH_ALPHA_INDEX);
}

void BrushStyleWidget::closeEvent(QCloseEvent *event)
{
	QWidget::closeEvent(event);
	itemDelegate->clearHoveredIndex();

	emit closed();
}

void BrushStyleWidget::leaveEvent(QEvent *event)
{
	// If visible we should turn off highlight of item
	if (isVisible())
		onItemHovered(QModelIndex());

	QWidget::leaveEvent(event);
}

void BrushStyleWidget::buildStyleList()
{
	int max = (getStyle() == BrushStyle::Size) ? BRUSH_SIZE_COUNT : BRUSH_ALPHA_COUNT;

	int currentRows = itemModel->rowCount();

	// Remove exceeding rows
	if (currentRows > max)
		itemModel->removeRows(max, currentRows - max);

	for (int i = 0; i < max; i++)
	{
		QStandardItem *item = itemModel->item(i);
		QString text = QString::number(revertIndex(i));

		if (item == NULL)
		{
			item = new QStandardItem(text);
			itemModel->setItem(i, item);
		}
		else
			item->setText(text);
	}
}

int BrushStyleWidget::revertIndex(int index) const
{
	int max = (getStyle() == BrushStyle::Size) ? BRUSH_SIZE_COUNT : BRUSH_ALPHA_COUNT;
	
	return max - index - 1;
}

void BrushStyleWidget::onItemSelected(const QModelIndex &index)
{
	int selectedIndex = index.row();

	if (getStyle() == BrushStyle::Size)
	{
		currentSizeIndex = selectedIndex;
		emit sizeSelected(revertIndex(selectedIndex));
	}
	else
	{
		currentAlphaIndex = selectedIndex;
		emit alphaSelected(revertIndex(selectedIndex));
	}

	close();
}

void BrushStyleWidget::onItemHovered(const QModelIndex &index)
{
	itemDelegate->setHoveredIndex(index);
}

// BrushStyleListItemDelegate class
void BrushStyleListItemDelegate::paint(QPainter *painter, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
	QPixmap itemPixmap;

	bool selected = false;
	
	// Hovered index prevails selection
	if (hoveredIndexSet)
		selected = index == hoveredIndex;
	else
		selected = option.state & QStyle::State_Selected;

	int value = index.data().toInt();

	if (parentStyleWidget->getStyle() == BrushStyle::Size)
	{
		itemPixmap = BrushStyle::getSizeSample(value, selected, parentStyleWidget->getColor());
	}
	else
	{
		itemPixmap = BrushStyle::getAlphaSample(value, parentStyleWidget->getSizeIndex(), selected);
	}
	QRect itemRect = option.rect;

	painter->drawPixmap(itemRect, itemPixmap);
}

QSize BrushStyleListItemDelegate::sizeHint(const QStyleOptionViewItem &option, const QModelIndex &index) const
{
	static QSize size = BrushStyle::getSizeSample(0, 0).size();

	Q_UNUSED(option)
	Q_UNUSED(index)

	return size;
}

QWidget *BrushStyleListItemDelegate::createEditor(QWidget *parent, const QStyleOptionViewItem &option, const QModelIndex &index)
{
	Q_UNUSED(parent)
	Q_UNUSED(option)
	Q_UNUSED(index)

	return NULL;
}

void BrushStyleListItemDelegate::setHoveredIndex(const QModelIndex &index)
{
	QModelIndex lastIndex = hoveredIndex;

	hoveredIndexSet = true;

	// If hover index was not set we need to clear current selection
	if (!lastIndex.isValid())
		lastIndex = parentStyleWidget->getStyleList()->currentIndex();

	hoveredIndex = QPersistentModelIndex(index);

	if (hoveredIndex == lastIndex)
		return;
	
	parentStyleWidget->getStyleList()->update(hoveredIndex);
	parentStyleWidget->getStyleList()->update(lastIndex);
}

void BrushStyleListItemDelegate::clearHoveredIndex()
{ 
	hoveredIndexSet = false;
	hoveredIndex = QModelIndex();
}

}