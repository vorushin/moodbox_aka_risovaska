#include "colorcontrol.h"

namespace MoodBox
{

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

void ColorControl::setColor(const QColor &color)
{
	this->color = color;
	update();
}

}
