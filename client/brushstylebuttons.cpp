#include "brushstylebuttons.h"

#include <QDesktopWidget>

#include "brushstylewidget.h"
#include "testtools.h"

namespace MoodBox
{

// BrushStyleButton class
BrushStyleButton::BrushStyleButton(QWidget *parent)
	: QToolButton(parent)
{
	TimeMeasure t("BrushStyleButton");

	setCheckable(true);
	setPopupMode(QToolButton::InstantPopup);
	
	styleWidget = new BrushStyleWidget(this);
	updateIcon();
	
	connect(this, SIGNAL(toggled(bool)), this, SLOT(onToggled(bool)));
	connect(this->styleWidget, SIGNAL(closed()), this, SLOT(onStyleWidgetClosed()));
}

QColor BrushStyleButton::getColor() const
{
	return styleWidget->getColor();
}

void BrushStyleButton::reset()
{
	styleWidget->reset();
}

void BrushStyleButton::setColor(const QColor &color)
{
	styleWidget->setColor(color);
	updateIcon();
}

void BrushStyleButton::updateIcon()
{
	if (styleWidget->getStyle() == BrushStyle::Size)
		setIcon(BrushStyle::getSizeSample(styleWidget->getSizeIndex(), false, styleWidget->getColor(), true));
	else
		setIcon(BrushStyle::getAlphaSample(styleWidget->getAlphaIndex(), styleWidget->getSizeIndex(), false, true));
}

void BrushStyleButton::onToggled(bool toggled)
{
	if (toggled)
	{
		QRect availableGeometry = QApplication::desktop()->availableGeometry(this);
		QPoint p = mapToGlobal(QPoint(0, height() + styleWidget->height()));
		if (p.y() > availableGeometry.bottom())
			styleWidget->move(mapToGlobal(QPoint(-1, -styleWidget->height())));
		else
			styleWidget->move(mapToGlobal(QPoint(-1, height())));
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

// BrushSizeButton class
BrushSizeButton::BrushSizeButton(QWidget *parent)
	: BrushStyleButton(parent)
{
	connect(styleWidget, SIGNAL(sizeSelected(int)), this, SIGNAL(sizeSelected(int)));
}

int BrushSizeButton::getSizeIndex() const
{
	return styleWidget->getSizeIndex();
}

void BrushSizeButton::setSizeIndex(int index)
{
	styleWidget->setSizeIndex(index);
	updateIcon();
}

// BrushAlphaButton class
BrushAlphaButton::BrushAlphaButton(QWidget *parent)
	: BrushStyleButton(parent)
{
	styleWidget->setStyle(BrushStyle::Alpha);

	connect(styleWidget, SIGNAL(alphaSelected(int)), this, SIGNAL(alphaSelected(int)));
}

int BrushAlphaButton::getAlphaIndex() const
{
	return styleWidget->getAlphaIndex();
}

void BrushAlphaButton::setAlphaIndex(int index)
{
	styleWidget->setAlphaIndex(index);
	updateIcon();
}

void BrushAlphaButton::setSizeIndex(int index)
{
	styleWidget->setSizeIndex(index);
	updateIcon();
}

}