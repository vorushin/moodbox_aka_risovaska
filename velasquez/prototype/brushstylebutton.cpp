#include "brushstylebutton.h"

#include "brushstyle.h"

// BrushStyleButton class
BrushStyleButton::BrushStyleButton(QWidget *parent)
	: QToolButton(parent)
{
	setCheckable(true);
	setPopupMode(QToolButton::MenuButtonPopup);
	
	styleWidget = new BrushStyleWidget(this);
	updateIcon();
	
	connect(this, SIGNAL(toggled(bool)), this, SLOT(onToggled(bool)));
	connect(this->styleWidget, SIGNAL(closed()), this, SLOT(onStyleWidgetClosed()));
}

BrushStyleButton::~BrushStyleButton()
{
}

BrushStyleWidget *BrushStyleButton::getStyleWidget()
{
	return styleWidget;
}

QColor BrushStyleButton::getColor() const
{
	return styleWidget->getColor();
}

void BrushStyleButton::setColor(const QColor &color)
{
	styleWidget->setColor(color);
	updateIcon();
}

void BrushStyleButton::onToggled(bool toggled)
{
	if (toggled)
	{
		styleWidget->move(mapToGlobal(QPoint(0, height())));
	}

	styleWidget->setVisible(toggled);
}

void BrushStyleButton::onStyleWidgetClosed()
{
	if (isChecked())
	{
		setChecked(false);
		updateIcon();
	}
}

void BrushStyleButton::updateIcon()
{
	setIcon(getBrushSample(styleWidget->getCurrentSize(), iconSize(), styleWidget->getCurrentAlpha(), getColor()));
}

// BrushSizeButton class
BrushSizeButton::BrushSizeButton(QWidget *parent)
	: BrushStyleButton(parent)
{
	connect(styleWidget, SIGNAL(sizeSelected(qreal)), this, SIGNAL(sizeSelected(qreal)));
}

qreal BrushSizeButton::getSize() const
{
	return styleWidget->getCurrentSize();
}

void BrushSizeButton::setSize(qreal size)
{
	styleWidget->setCurrentSize(size);
	updateIcon();
}

// BrushAlphaButton class
BrushAlphaButton::BrushAlphaButton(QWidget *parent)
	: BrushStyleButton(parent)
{
	styleWidget->setStyle(BrushStyleWidget::Alpha);

	connect(styleWidget, SIGNAL(alphaSelected(qreal)), this, SIGNAL(alphaSelected(qreal)));
}

qreal BrushAlphaButton::getAlpha() const
{
	return styleWidget->getCurrentAlpha();
}

void BrushAlphaButton::setAlpha(qreal alpha)
{
	styleWidget->setCurrentAlpha(alpha);
	updateIcon();
}

void BrushAlphaButton::setSize(qreal size)
{
	styleWidget->setCurrentSize(size);
	updateIcon();
}
