#include "colorwheelwidget.h"

#include <QPainter>
#include <QRegion>
#include <QMouseEvent>

#define _USE_MATH_DEFINES

#include <math.h>

#include "drawingutils.h"

namespace MoodBox
{

using namespace Velasquez;

// Simple HSV structure for buffer
struct HSV
{
	int h, s, v;
	bool empty;

	inline HSV(int h, int s, int v)
	{
		this->h = h;
		this->s = s;
		this->v = v;

		empty = false;
	};

	inline HSV()
	{
		h = 0;
		v = 0;
		s = 0;

		empty = true;
	};

	inline HSV(const QColor &color)
	{
		h = color.hue();
		s = color.saturation();
		v = color.value();

		empty = false;
	}
};

ColorWheelWidget::ColorWheelWidget(QWidget *parent)
	: ColorSelectionControl(parent), radius(0), buffer(NULL)
{
	this->horizontalSpacing = 5;
	this->verticalSpacing = 5;
}

ColorWheelWidget::ColorWheelWidget(const QColor &color, QWidget *parent)
	: ColorSelectionControl(color, parent), radius(0), buffer(NULL)
{
	this->horizontalSpacing = 5;
	this->verticalSpacing = 5;
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

	painter.drawImage(maskRect, getColorWheelMask());

	painter.setRenderHint(QPainter::Antialiasing);

	painter.setPen(Qt::darkGray);
	painter.setBrush(Qt::transparent);
	painter.drawEllipse(colorPoint.x() + imageRect.left(), colorPoint.y() + imageRect.top(), COLOR_WHEEL_MARK_INNER_RADIUS * 2, COLOR_WHEEL_MARK_INNER_RADIUS * 2);

	painter.setPen(Qt::white);
	painter.setBrush(Qt::transparent);
	painter.drawEllipse(colorPoint.x() + imageRect.left() - 1, colorPoint.y() + imageRect.top() - 1, COLOR_WHEEL_MARK_OUTER_RADIUS * 2, COLOR_WHEEL_MARK_OUTER_RADIUS * 2);
}

void ColorWheelWidget::resizeEvent(QResizeEvent *event)
{
	resetBuffer();
	ColorSelectionControl::resizeEvent(event);
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
	qreal x = point.x() - ((qreal)imageRect.width()) / 2;
	qreal y = ((qreal)imageRect.height()) / 2 - point.y();
	
	qreal angle = atan2(y, x) * 180 / M_PI;

	// HSV doesn't like negative angles
	angle = DrawingUtils::normalizeAngle(angle);

	qreal h = sqrt(x*x + y*y);

	return getPixelColor(h, angle);
}

QPointF ColorWheelWidget::getColorPosition(const QColor &color) const
{
	qreal angle = color.hue();
	qreal h = color.saturationF() * radius;

	QPointF pos;
	pos.setX(cos(M_PI / 180.0 * angle ) * h);
	pos.setY(sin(M_PI / 180.0 * angle ) * h);

	pos.rx() += ((qreal)imageRect.width()) / 2 - COLOR_WHEEL_MARK_INNER_RADIUS;
	pos.ry() = ((qreal)imageRect.height()) / 2 - COLOR_WHEEL_MARK_INNER_RADIUS - pos.y();

	return pos;
}

void ColorWheelWidget::resetImage()
{
	ColorSelectionControl::resetImage();
	radius = qMin(imageRect.width(), imageRect.height()) / 2;
}

void ColorWheelWidget::createImage()
{
	if (buffer == NULL)
		createBuffer();

	image = QImage(imageRect.width(), imageRect.height(), QImage::Format_ARGB32_Premultiplied);
	image.fill(0);

	QRect rect(0, 0, imageRect.width(), imageRect.height());

	QPoint center(rect.width() / 2, rect.height() / 2);

	for (int y = 0; y < rect.height(); y++)
		for (int x = 0; x < rect.width(); x++)
		{
			HSV hsv = buffer[x + y * rect.width()];

			if (!hsv.empty)
			{
				QColor color = QColor::fromHsv(hsv.h, hsv.s, hsv.v);
				image.setPixel(x, y, color.rgb());
			}
		}

	maskRect = imageRect;
	maskRect.adjust(-3, -3, 4, 4);
}

void ColorWheelWidget::createBuffer()
{
	if (buffer != NULL)
		resetBuffer();

	// Get memory
	buffer = (HSV *) calloc(imageRect.width() * imageRect.height(), sizeof(HSV));

	// adjust to center
	QPointF center( ((qreal)imageRect.width()) / 2, ((qreal)imageRect.height()) / 2);
	
	for (int y = 0; y < imageRect.height(); y++)
		for (int x = 0; x < imageRect.width(); x++)
		{
			qreal d = DrawingUtils::segmentLength(QPoint(x, y), center);

			HSV hsv;

			if (d <= radius)
			{
				qreal nx = x - center.x();
				qreal ny = center.y() - y;
				qreal angle = atan2(ny, nx) * 180 / M_PI;

				angle = DrawingUtils::normalizeAngle(angle);

				hsv = HSV(getPixelColor(d, angle));
			}

			buffer[x + y * imageRect.width()]  = hsv;
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

	for (int y = 0; y < imageRect.height(); y++)
		for (int x = 0; x < imageRect.width(); x++)
		{
			HSV hsv = buffer[x + y * imageRect.width()];

			if (!hsv.empty)
				hsv.v = color.value();

			buffer[x + y * imageRect.width()] = hsv;
		}

	resetImage();
}

QImage ColorWheelWidget::getColorWheelMask()
{
	static QImage colorWheelMask(COLOR_WHEEL_WIDGET_MASK);

	return colorWheelMask;
}

}