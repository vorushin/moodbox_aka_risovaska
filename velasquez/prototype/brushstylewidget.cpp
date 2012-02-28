#include "brushstylewidget.h"

#include <QVariant>
#include <QStandardItem>

#include "brushstyle.h"

#define STYLE_ROLE	Qt::UserRole + 1

BrushStyleWidget::BrushStyleWidget(QWidget *parent)
: QWidget(parent, Qt::Popup), style(Size), color(Qt::black)
{
	setupUi(this);

	itemModel = new QStandardItemModel(this);
	itemModel->setColumnCount(1);
	styleList->setModel(itemModel);

	currentSize = BrushSizes[DEFAULT_BRUSH_SIZE_INDEX];
	currentAlpha = BrushAlphas[DEFAULT_BRUSH_ALPHA_INDEX];

	buildStyleList();

	connect(styleList, SIGNAL(clicked(const QModelIndex &)), this, SLOT(onItemSelected()));
}

BrushStyleWidget::~BrushStyleWidget()
{
}

void BrushStyleWidget::setStyle(Style style)
{
	if (this->style == style)
		return;

	this->style = style;
	buildStyleList();
}

BrushStyleWidget::Style BrushStyleWidget::getStyle() const
{
	return this->style;
}

void BrushStyleWidget::setCurrentSize(qreal size)
{
	if (getStyle() == Alpha)
	{
		currentSize = size;		

		buildStyleList();
	}
	else
		setCurrentStyleValue(size);
}

qreal BrushStyleWidget::getCurrentSize() const
{
	return (getStyle() == Alpha) ? currentSize : getCurrentStyleValue();
}

void BrushStyleWidget::setCurrentAlpha(qreal alpha)
{
	if (getStyle() == Size)
	{
		currentAlpha = alpha;

		buildStyleList();
	}
	else
		setCurrentStyleValue(alpha);
}

qreal BrushStyleWidget::getCurrentAlpha() const
{
	return (getStyle() == Size) ? currentAlpha : getCurrentStyleValue();
}

void BrushStyleWidget::closeEvent(QCloseEvent *event)
{
	QWidget::closeEvent(event);

	emit closed();
}

void BrushStyleWidget::setColor(const QColor &color)
{
	this->color = color;

	qreal currentValue = getCurrentStyleValue();

	buildStyleList();

	setCurrentStyleValue(currentValue);
}

QColor BrushStyleWidget::getColor() const
{
	return this->color;
}

void BrushStyleWidget::onItemSelected()
{
	if (getStyle() == Size)
	{
		emit sizeSelected(getCurrentStyleValue());
	}
	else
	{
		emit alphaSelected(getCurrentStyleValue());
	}

	close();
}

void BrushStyleWidget::buildStyleList()
{
	itemModel->clear();

	int max = (getStyle() == Size) ? BRUSH_SIZE_COUNT : BRUSH_ALPHA_COUNT;

	for (int i = 0; i < max; i++)
	{
		qreal value = (getStyle() == Size) ? BrushSizes[i] : BrushAlphas[i];
		QPixmap pixmap;

		if (getStyle() == Size)
		{
			pixmap = getBrushSample(value, styleList->iconSize(), getCurrentAlpha(), getColor());
		}
		else
		{
			pixmap = getBrushSample(getCurrentSize(), styleList->iconSize(), value, getColor());
		}

		QStandardItem *item = new QStandardItem(pixmap, QString());
		item->setData(value, STYLE_ROLE);

		itemModel->appendRow(item);
	}
}

void BrushStyleWidget::setCurrentStyleValue(qreal value)
{
   for (int i = 0; i < itemModel->rowCount(); i++)
   {
		QVariant data = itemModel->item(i)->data(STYLE_ROLE);

		if (data.toDouble() == value)
			styleList->setCurrentIndex(itemModel->indexFromItem(itemModel->item(i)));
   }
}

qreal BrushStyleWidget::getCurrentStyleValue() const
{
	if (styleList->currentIndex().isValid())
	{
		QVariant data = itemModel->itemFromIndex(styleList->currentIndex())->data(STYLE_ROLE);

		return data.toDouble();
	}
	else
		// Get defaults if index is invalid
		return (getStyle() == Size) ? currentSize : currentAlpha;
}
