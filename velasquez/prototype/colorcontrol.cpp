#include "colorcontrol.h"

#include <math.h>

ColorControl::ColorControl(QWidget *parent)
	: QWidget(parent)
{
	this->color = QColor(Qt::white);
}

ColorControl::ColorControl(const QColor &color, QWidget *parent)
	: QWidget(parent)
{
	this->color = color;
}

QColor ColorControl::getColor() const
{
	return this->color;
}

void ColorControl::setColor(const QColor &color)
{
	this->color = color;
	update();
}

inline qreal getLength(QPointF p)
{
	qreal x = p.x();
	qreal y = p.y();

	return sqrt(x*x + y*y);
}

qreal getSegmentLength(const QPointF &p1, const QPointF &p2)
{
	return getLength(p1 - p2);
}
