#include "colorselectioncontrol.h"

#include <QPainter>
#include <QMouseEvent>

namespace MoodBox
{

ColorSelectionControl::ColorSelectionControl(QWidget *parent)
	: ColorControl(parent), horizontalSpacing(0), verticalSpacing(0)
{
	imageRect = QRect(0, 0, width(), height());
}

ColorSelectionControl::ColorSelectionControl(const QColor &color, QWidget *parent)
	: ColorControl(color, parent), horizontalSpacing(0), verticalSpacing(0)
{
	imageRect = QRect(0, 0, width(), height());
}

void ColorSelectionControl::setColor(const QColor &color)
{
	if (this->color == color)
		return;

	this->color = color;	

	updateColorPoint();
	update();

	emit colorSelected(this->color);
}

QPoint ColorSelectionControl::eventToImage(const QPoint &eventPos) const
{
	QPoint imagePoint(eventPos - imageRect.topLeft());

	if (eventPos.x() < imageRect.left())
		imagePoint.setX(imageRect.left());
	else
		if (eventPos.x() > imageRect.right())
			imagePoint.setX(imageRect.right());

	if (eventPos.y() < imageRect.top())
		imagePoint.setY(imageRect.top());
	else
		if (eventPos.y() > imageRect.bottom())
			imagePoint.setY(imageRect.bottom());

	return imagePoint;
}

void ColorSelectionControl::paintEvent(QPaintEvent *event)
{
	Q_UNUSED(event)

	QPainter painter(this);
	painter.setRenderHint(QPainter::Antialiasing);

	if (image.isNull())
		createImage();
	
	painter.drawImage(imageRect, image);
}

void ColorSelectionControl::resizeEvent(QResizeEvent *event)
{
	ColorControl::resizeEvent(event);

	updateImageRect();
}

void ColorSelectionControl::mousePressEvent(QMouseEvent *event)
{
	if (event->button() == Qt::LeftButton)
	{
		setColor(getPixelColor(eventToImage(event->pos())));
		event->accept();

		return;
	}

	QWidget::mousePressEvent(event);
}

void ColorSelectionControl::mouseMoveEvent(QMouseEvent *event)
{
	if (event->buttons() & Qt::LeftButton)
	{
		setColor(getPixelColor(eventToImage(event->pos())));
		event->accept();

		return;
	}

	QWidget::mouseMoveEvent(event);
}

void ColorSelectionControl::setSpacing(int horizontal, int vertical)
{
	if (horizontal == this->horizontalSpacing && vertical == this->verticalSpacing)
		return;

	this->horizontalSpacing = horizontal;
	this->verticalSpacing = vertical;

	updateImageRect();
}

void ColorSelectionControl::updateImageRect()
{
	imageRect = QRect(horizontalSpacing, verticalSpacing, width() - horizontalSpacing * 2, height() - verticalSpacing * 2);

	resetImage();
	updateColorPoint();
}

void ColorSelectionControl::resetImage()
{
	if (!this->image.isNull())
		this->image = QImage();
}

void ColorSelectionControl::updateColorPoint()
{
	this->colorPoint = getColorPosition(this->color);
}

}