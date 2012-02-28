#include "colorvolumewidget.h"

#include <QPainter>
#include <QLinearGradient>

ColorVolumeWidget::ColorVolumeWidget(QWidget *parent)
	: ColorSelectionControl(parent), direction(Horizontal), mode(DarkOnly)
{
}

ColorVolumeWidget::ColorVolumeWidget(const QColor &color, QWidget *parent)
	: ColorSelectionControl(color, parent), direction(Horizontal), mode(DarkOnly)
{
}

void ColorVolumeWidget::setDirection(Direction direction)
{
	if (this->direction == direction)
		return;

	this->direction = direction;
	
	resetImage();
	update();
}

ColorVolumeWidget::Direction ColorVolumeWidget::getDirection() const
{
	return this->direction;
}

void ColorVolumeWidget::setMode(Mode mode)
{
	if (this->mode == mode)
		return;

	this->mode = mode;
	
	resetImage();
	updateColorPoint();

	update();
}

ColorVolumeWidget::Mode ColorVolumeWidget::getMode() const
{
	return this->mode;
}

void ColorVolumeWidget::setColor(const QColor &color)
{
	if (this->color != color)
		resetImage();

	ColorSelectionControl::setColor(color);
}

void ColorVolumeWidget::paintEvent(QPaintEvent *event)
{
	ColorSelectionControl::paintEvent(event);

	QPainter painter(this);
	painter.setRenderHint(QPainter::Antialiasing);

	painter.setPen(Qt::black);
	painter.setBrush(Qt::white);

	if (this->direction == Horizontal)
		painter.drawRect(colorPoint.x() - COLOR_VOLUME_SLIDER_WIDTH / 2 , 0, COLOR_VOLUME_SLIDER_WIDTH, height());
	else
		painter.drawRect(0, colorPoint.y() - COLOR_VOLUME_SLIDER_WIDTH / 2 , width(), COLOR_VOLUME_SLIDER_WIDTH);
}

QColor ColorVolumeWidget::getPixelColor(const QPoint &point) const
{
	qreal size = (this->direction == Horizontal) ? width() : height();
	qreal pos = (this->direction == Horizontal) ? point.x() : point.y();

	qreal h = this->color.hue(), s = this->color.saturation(), v;

	if (this->mode == DarkOnly)
	{
		v = 255.0 / size * pos;		
	}
	else
	{
		if (pos < size / 2)
		{
			v = 255.0 / (size / 2) * (pos);
		}
		else
		{
			s = 255 - 255.0 / (size / 2) * (pos - size / 2);
			v = 255.0;
		}
	}

	return QColor::fromHsv(h, qMin(255.0, qMax(s, 0.0)), qMin(255.0, qMax(v, 0.0)));
}

QPointF ColorVolumeWidget::getColorPosition(const QColor &color) const
{
	qreal size = (this->direction == Horizontal) ? width() : height();
	qreal pos;

	if (this->mode == DarkOnly)
	{
		pos = ((qreal)color.value()) / 255 * size;
	}
	else
	{
		if (color.value() < 255)
		{
			pos = ((qreal)color.value()) / 255 * (size / 2);
		}
		else
		{
			pos = (255.0 - (qreal)color.saturation()) / 255 * (size / 2) + size / 2;
		}
	}

	return (this->direction == Horizontal) ? QPointF(pos, 0) : QPointF(0, pos);
}

void ColorVolumeWidget::createImage()
{
	image = QImage(width(), height(), QImage::Format_ARGB32_Premultiplied);

	QPainter painter(&image);

	painter.initFrom(this);
	painter.setRenderHint(QPainter::Antialiasing);
	painter.setPen(Qt::NoPen);
		
	int h, s, v;
	this->color.getHsv(&h, &s, &v);

	QColor lightColor, mediumColor, darkColor;
	lightColor.setHsv(h, 0, 255);
	mediumColor.setHsv(h, s, 255);
	darkColor.setHsv(h, s, 0);

	QLinearGradient gradient;

	if (this->direction == Vertical)
	{
		gradient = QLinearGradient(rect().center().x(), 0, rect().center().x(), rect().bottom());
	}
	else
	{
		gradient = QLinearGradient(0, rect().center().y(), rect().right(), rect().center().y());
	}

	if (this->mode == DarkOnly)
	{
		gradient.setColorAt(0.0, darkColor);
		gradient.setColorAt(1.0, mediumColor);
	}
	else
	{
		gradient.setColorAt(0.0, darkColor);
		gradient.setColorAt(0.5, mediumColor);
		gradient.setColorAt(1.0, lightColor);
	}

	painter.fillRect(0, 0, rect().width(), rect().height(), gradient);
	
	painter.end();
}
