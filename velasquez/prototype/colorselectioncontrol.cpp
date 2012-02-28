#include "colorselectioncontrol.h"

#include <QPainter>
#include <QMouseEvent>

ColorSelectionControl::ColorSelectionControl(QWidget *parent)
	: ColorControl(parent)
{
}

ColorSelectionControl::ColorSelectionControl(const QColor &color, QWidget *parent)
	: ColorControl(color, parent)
{
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

void ColorSelectionControl::paintEvent(QPaintEvent *)
{
	QPainter painter(this);
	painter.setRenderHint(QPainter::Antialiasing);

	if (image.isNull())
		createImage();

	painter.drawImage(0, 0, image);
}

void ColorSelectionControl::resizeEvent(QResizeEvent *event)
{
	ColorControl::resizeEvent(event);

	resetImage();
	updateColorPoint();
}

void ColorSelectionControl::mousePressEvent(QMouseEvent *event)
{
	if (event->button() == Qt::LeftButton)
	{
		setColor(getPixelColor(event->pos()));
	}

	QWidget::mousePressEvent(event);
}

void ColorSelectionControl::mouseMoveEvent(QMouseEvent *event)
{
	if (event->buttons() & Qt::LeftButton)
	{
		setColor(getPixelColor(event->pos()));
	}

	QWidget::mouseMoveEvent(event);
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
