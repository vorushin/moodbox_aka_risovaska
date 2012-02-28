#include "colorvolumewidget.h"

#include <QPainter>
#include <QLinearGradient>

namespace MoodBox
{

ColorVolumeWidget::ColorVolumeWidget(QWidget *parent)
	: ColorSelectionControl(parent), direction(Horizontal), mode(DarkOnly)
{
	this->horizontalSpacing = 5;
	this->verticalSpacing = 3;
}

ColorVolumeWidget::ColorVolumeWidget(const QColor &color, QWidget *parent)
	: ColorSelectionControl(color, parent), direction(Horizontal), mode(DarkOnly)
{
	this->horizontalSpacing = 5;
	this->verticalSpacing = 3;
}

void ColorVolumeWidget::setDirection(Direction direction)
{
	if (this->direction == direction)
		return;

	this->direction = direction;
	
	resetImage();
	updateColorPoint();

	update();
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
	painter.setPen(Qt::NoPen);
	painter.setBrush(this->color);
		
	if (this->direction == Horizontal)
	{
		painter.drawImage(QPoint(colorPoint.x() + imageRect.left() - 4, 0), getSliderMaskCopy());

		painter.drawImage(QPoint(imageRect.left() - 3, -1), getVolumeMask());

		painter.drawRect(colorPoint.x() + imageRect.left() - COLOR_VOLUME_SLIDER_WIDTH / 2 , 0, COLOR_VOLUME_SLIDER_WIDTH, height());

		painter.drawImage(QPoint(colorPoint.x() + imageRect.left() - 4, 0), getSliderMaskMultiply());
	}
	else
	{
		painter.drawRect(0, colorPoint.y() + imageRect.top() - COLOR_VOLUME_SLIDER_WIDTH / 2 , width(), COLOR_VOLUME_SLIDER_WIDTH);
	}
}

QColor ColorVolumeWidget::getPixelColor(const QPoint &point) const
{
	qreal size = (this->direction == Horizontal) ? imageRect.width() : imageRect.height();
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
	qreal size = (this->direction == Horizontal) ? imageRect.width() : imageRect.height();
	qreal pos;

	if (this->mode == DarkOnly)
	{
		pos = color.valueF() * size;
	}
	else
	{
		if (color.value() < 255)
		{
			pos = (color.valueF()) * (size / 2);
		}
		else
		{
			pos = (255.0 - color.saturation()) / 255 * (size / 2) + size / 2;
		}
	}

	return (this->direction == Horizontal) ? QPointF(pos, 0) : QPointF(0, pos);
}

void ColorVolumeWidget::createImage()
{
	image = QImage(imageRect.width(), imageRect.height(), QImage::Format_ARGB32_Premultiplied);
	QRect rect = QRect(0, 0, imageRect.width(), imageRect.height());

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
		gradient = QLinearGradient(rect.center().x(), 0, rect.center().x(), rect.bottom());
	}
	else
	{
		gradient = QLinearGradient(0, rect.center().y(), rect.right(), rect.center().y());
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

	painter.fillRect(rect, gradient);
	
	painter.end();
}

QImage ColorVolumeWidget::getSliderMaskCopy()
{
	static QImage MaskImage(COLOR_VOLUME_WIDGET_SLIDER_MASK);

	return MaskImage.copy();
}

QImage ColorVolumeWidget::getSliderMaskMultiply()
{
	static QImage MaskMultiplyImage(COLOR_VOLUME_WIDGET_SLIDER_MASK_MULTIPLY);

	return MaskMultiplyImage;
}

QImage ColorVolumeWidget::getVolumeMask()
{
	static QImage VolumeMaskImage(COLOR_VOLUME_WIDGET_MASK);

	return VolumeMaskImage;
}

}