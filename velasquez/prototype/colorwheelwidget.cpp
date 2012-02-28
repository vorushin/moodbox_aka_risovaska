#include "colorwheelwidget.h"

#include <QPainter>
#include <QRegion>
#include <QMouseEvent>

#define _USE_MATH_DEFINES

#include <math.h>

ColorWheelWidget::ColorWheelWidget(QWidget *parent)
	: ColorSelectionControl(parent), radius(0), buffer(NULL)
{
}

ColorWheelWidget::ColorWheelWidget(const QColor &color, QWidget *parent)
	: ColorSelectionControl(color, parent), radius(0), buffer(NULL)
{
}

ColorWheelWidget::~ColorWheelWidget()
{
	resetBuffer();
}

void ColorWheelWidget::setColor(const QColor &color)
{
	int oldV = this->color.value();
	
	ColorSelectionControl::setColor(color);

	if (oldV != this->color.value())
		updateBufferValue();
}

void ColorWheelWidget::paintEvent(QPaintEvent *event)
{
	ColorSelectionControl::paintEvent(event);

	QPainter painter(this);
	painter.setRenderHint(QPainter::Antialiasing);

	painter.setPen(Qt::black);
	painter.setBrush(Qt::white);

	painter.drawEllipse(colorPoint.x(), colorPoint.y(), COLOR_WHEEL_MARK_RADIUS * 2, COLOR_WHEEL_MARK_RADIUS * 2);
}

void ColorWheelWidget::resizeEvent(QResizeEvent *event)
{
	updateRadius();
	resetBuffer();

	ColorSelectionControl::resizeEvent(event);

    QRegion maskedRegion(width() / 2 - radius, height() / 2 - radius, radius * 2,
                          radius * 2, QRegion::Ellipse);

    setMask(maskedRegion);
}

QColor ColorWheelWidget::getPixelColor(qreal radialPos, qreal angle) const
{
	int hue = qMin(angle, 359.0);
	int sat = qMin(255.0 / radius * radialPos, 255.0);
	int vol = color.value();

	return QColor::fromHsv(hue, sat, vol);
}

QColor ColorWheelWidget::getPixelColor(const QPoint &point) const
{
	qreal x = point.x() - ((qreal)width()) / 2;
	qreal y = ((qreal)height()) / 2 - point.y();
	
	qreal angle = atan2(y, x) * 180 / M_PI;

	// HSV doesn't like negative angles
	if (angle < 0)
		angle += 360;

	qreal h = sqrt(x*x + y*y);

	return getPixelColor(h, angle);
}

QPointF ColorWheelWidget::getColorPosition(const QColor &color) const
{
	qreal angle = color.hue();
	qreal h = ((qreal)color.saturation()) / 255 * radius;

	QPointF pos;
	pos.setX(cos(M_PI / 180.0 * angle ) * h);
	pos.setY(sin(M_PI / 180.0 * angle ) * h);

	pos.rx() += ((qreal)width()) / 2 - COLOR_WHEEL_MARK_RADIUS;
	pos.ry() = ((qreal)height()) / 2 - COLOR_WHEEL_MARK_RADIUS - pos.y();

	return pos;
}

void ColorWheelWidget::createImage()
{
	if (buffer == NULL)
		createBuffer();

	image = QImage(width(), height(), QImage::Format_ARGB32_Premultiplied);

	QPoint center(width() / 2, height() / 2);

	for (int y = 0; y < height(); y++)
		for (int x = 0; x < width(); x++)
		{
			HSV hsv = buffer[x + y * width()];

			if (!hsv.empty)
			{
				QColor color = QColor::fromHsv(hsv.h, hsv.s, hsv.v);
				image.setPixel(x, y, color.rgb());
			}
		}
}

void ColorWheelWidget::updateRadius()
{
	radius = qMin(width(), height()) / 2;
}

void ColorWheelWidget::createBuffer()
{
	if (buffer != NULL)
		resetBuffer();

	// Get memory
	buffer = (HSV *) calloc(width() * height(), sizeof(HSV));

	// adjust to center
	QPointF center( ((qreal)width()) / 2, ((qreal)height()) / 2);
	
	for (int y = 0; y < height(); y++)
		for (int x = 0; x < width(); x++)
		{
			qreal d = getSegmentLength(QPoint(x, y), center);

			HSV hsv;

			if (d <= radius)
			{
				qreal nx = x - center.x();
				qreal ny = center.y() - y;
				qreal angle = atan2(ny, nx) * 180 / M_PI;

				if (angle < 0)
					angle += 360;

				hsv = HSV(getPixelColor(d, angle));
			}

			buffer[x + y * width()]  = hsv;
		}
}

void ColorWheelWidget::resetBuffer()
{
	if (buffer == NULL)
		return;

	free(buffer);
	buffer = NULL;

	resetImage();
}

void ColorWheelWidget::updateBufferValue()
{
	if (buffer == NULL)
		return;

	for (int y = 0; y < height(); y++)
		for (int x = 0; x < width(); x++)
		{
			HSV hsv = buffer[x + y * width()];

			if (!hsv.empty)
				hsv.v = color.value();

			buffer[x + y * width()] = hsv;
		}

	resetImage();
}
