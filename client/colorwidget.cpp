#include "colorwidget.h"

#include <QPainter>
#include <QMouseEvent>
#include <QLinearGradient>

namespace MoodBox
{

ColorWidget::ColorWidget(QWidget *parent)
	: ColorControl(parent), index(-1), active(true), readOnly(false), hovered(false)
{
	setMouseTracking(true);
}

ColorWidget::ColorWidget(const QColor &color, int index, QWidget *parent)
	: ColorControl(color, parent), active(false), readOnly(false), hovered(false)
{
	this->index = index;
	setMouseTracking(true);
}

void ColorWidget::setIndex(int index)
{
	this->index = index;
}

void ColorWidget::setActive(bool active)
{
	this->active = active;
	update();
}

void ColorWidget::setReadOnly(bool readOnly)
{
	if (this->readOnly == readOnly)
		return;

	this->readOnly = readOnly;
	update();
}

QSize ColorWidget::sizeHint() const
{
	return QSize(COLOR_WIDGET_WIDTH, COLOR_WIDGET_HEIGHT);
}

void ColorWidget::paintEvent(QPaintEvent *)
{
	QPainter painter(this);
	painter.setPen(Qt::NoPen);
	QBrush brush(this->color, Qt::SolidPattern);
	QRect r(0, 0, width(), height());

	if (getActive())
	{
		QImage img = getSelectedMask();
		QPainter imgPainter(&img);
		imgPainter.setCompositionMode(QPainter::CompositionMode_Multiply);
		imgPainter.fillRect(img.rect(), brush);
		painter.drawImage(QPoint(0, 0), img);
	}
	else 
	if (hovered && !readOnly)
	{
		QImage img = getHoverMask();
		QPainter imgPainter(&img);
		imgPainter.setCompositionMode(QPainter::CompositionMode_DestinationAtop);
		imgPainter.fillRect(img.rect(), brush);
		painter.drawImage(QPoint(0, 0), img);
	}
	else
	{
		QImage img = getNormalMask();
		QPainter imgPainter(&img);
		imgPainter.setCompositionMode(QPainter::CompositionMode_Multiply);
		imgPainter.fillRect(img.rect(), brush);
		painter.drawImage(QPoint(0, 0), img);
	}
}

void ColorWidget::mousePressEvent(QMouseEvent *event)
{
	if (event->button() == Qt::LeftButton && !readOnly)
	{
		event->accept();

		if (!getActive())
			emit activationRequest(this);
		else
			emit editRequest(this);

		return;
	}

	QWidget::mousePressEvent(event);
}

void ColorWidget::enterEvent(QEvent *event)
{
	hovered = true;
	update();

	ColorControl::enterEvent(event);
}

void ColorWidget::leaveEvent(QEvent *event)
{
	hovered = false;
	update();

	ColorControl::leaveEvent(event);
}

QImage ColorWidget::getHoverMask()
{
	static QImage hoverImage(COLOR_WIDGET_HOVER_MASK);

	return hoverImage;
}

QImage ColorWidget::getSelectedMask()
{
	static QImage selectedImage(COLOR_WIDGET_SELECTED_MASK);

	return selectedImage;
}

QImage ColorWidget::getNormalMask()
{
	static QImage normalImage(COLOR_WIDGET_NORMAL_MASK);

	return normalImage;
}

}
