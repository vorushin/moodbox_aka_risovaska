#include "colorwidget.h"

#include <QPainter>
#include <QMouseEvent>
#include <QRadialGradient>

ColorWidget::ColorWidget(QWidget *parent)
	: ColorControl(parent), index(-1), active(false)
{
}

ColorWidget::ColorWidget(const QColor &color, int index, QWidget *parent)
	: ColorControl(color, parent), active(false)
{
	this->index = index;
}

void ColorWidget::setIndex(int index)
{
	this->index = index;
}

int ColorWidget::getIndex() const
{
	return this->index;
}

void ColorWidget::setActive(bool active)
{
	this->active = active;
	update();
}

bool ColorWidget::getActive() const
{
	return this->active;
}

QSize ColorWidget::sizeHint() const
{
	return QSize(16, 16);
}

void ColorWidget::paintEvent(QPaintEvent *)
{
	QPainter painter(this);
	QRect r(0, 0, width(), height());

	painter.setPen(Qt::NoPen);

	if (getActive())
	{
		QRadialGradient gradient(r.center(), width() * 2);
		gradient.setColorAt(0, this->color);
		gradient.setColorAt(1, this->color.darker());

		painter.setBrush(gradient);
	}
	else
		painter.setBrush(this->color);

	painter.drawRect(r);
}

void ColorWidget::mousePressEvent(QMouseEvent *event)
{
	if (event->button() == Qt::LeftButton)
	{
		if (!getActive())
			emit activationRequest(this);
		else
			emit editRequest(this);
	}

	QWidget::mousePressEvent(event);
}
