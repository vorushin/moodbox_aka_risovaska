#include "oilbrushelement.h"

#include <QPainter>

#include "debug.h"
#include "drawingutils.h"

namespace Velasquez
{

OilBrushElement::OilBrushElement(QGraphicsItem *parent) 
	: BrushDrawingElement(parent)
{
	currentColor = DEFAULT_OILBRUSH_COLOR;
	currentWidth = DEFAULT_OILBRUSH_WIDTH;
	currentAlpha = currentColor.alphaF();

	setInterpolateMode(Excessive);
}

qint32 OilBrushElement::getType() const
{
	return Type;
}

void OilBrushElement::setColor(const QColor &color)
{
	QColor newColor = color;

	newColor = DrawingUtils::addRandomColorDeviation(color, OILBRUSH_COLORDEV_HUE_RANGE, OILBRUSH_COLORDEV_SAT_RANGE, OILBRUSH_COLORDEV_VAL_RANGE);

	BrushDrawingElement::setColor(newColor);
}

void OilBrushElement::setAlpha(qreal alpha)
{
	int pos = qMin((int)alpha, OILBRUSH_TRANSPARENCY_COUNT - 1);
		
	alpha = OilBrushTransparencies[pos];

	BrushDrawingElement::setAlpha(alpha);
}

void OilBrushElement::setWidth(qreal width)
{
	width = width * OILBRUSH_WIDTH_SCALE + 5;

	BrushDrawingElement::setWidth(width);
}

qreal OilBrushElement::getPaintTransparency(qreal speed) const
{
	static const qreal maxSpeed = 2.0;

	// t=t(s) values, first is speed and second is t
	static const qreal ranges[4][2] = { {0, 0.5}, {0.3, 0.2}, {1.0, 0.1}, {maxSpeed, 0.05} };
	
	// t=k*s + b, coefficients
	static qreal coefficients[4][2];
	static bool coefficientsReady[4] = { false, false, false, false };

	qreal limitedSpeed = qMin(speed, maxSpeed);
	
	int i = 1;

	for (; i <= 3; i++)
	{
		if (limitedSpeed <= ranges[i][0])
			break;
	}

	if (!coefficientsReady[i])
	{
		qreal t1 = ranges[i-1][1];
		qreal t2 = ranges[i][1];

		qreal s1 = ranges[i-1][0];
		qreal s2 = ranges[i][0];

		coefficients[i][0] = (t2 - t1)/(s2 - s1);
		coefficients[i][1] = t1 - coefficients[i][0] * s1;
		
		coefficientsReady[i] = true;
	}

	qreal transparency = coefficients[i][0] * limitedSpeed + coefficients[i][1];

	return transparency;
}

qreal OilBrushElement::getPaintSize(qreal speed) const
{
	static const qreal maxSpeed = 2.0;

	const qreal limitedSpeed = qMin(speed, maxSpeed) / maxSpeed;

	// Dependency from speed
	qreal size = currentWidth * (1 - limitedSpeed * 0.5);

	// Minimal size
	return qMax(size, currentWidth * 0.3);
}

void OilBrushElement::updateAlphaImage()
{
	alphaImage = QImage(OILBRUSH_IMAGE).scaled(currentWidth, currentWidth, Qt::KeepAspectRatio, Qt::SmoothTransformation).alphaChannel();
}

void OilBrushElement::configureStrokesPainter(QPainter *painter)
{
	BrushDrawingElement::configureStrokesPainter(painter);

	updateAlphaImage();	
}

void OilBrushElement::paintStroke(QPainter *painter, int index)
{
	BrushStroke stroke = strokes.value(index);

	// Set alpha
	QColor imageColor = currentColor;
	imageColor.setAlphaF(currentAlpha * getPaintTransparency(stroke.getSpeed()));

	// Create image
	QImage strokeImage = QImage(alphaImage.width(), alphaImage.height(), QImage::Format_ARGB32);
	strokeImage.fill(imageColor.rgba());

	// Apply alpha
	strokeImage.setAlphaChannel(alphaImage);

	// Apply scale and transformation
	qint32 size = getPaintSize(stroke.getSpeed());
	strokeImage = strokeImage.transformed(QTransform().rotate(stroke.getAngle()));
	strokeImage = strokeImage.scaled(size, size, Qt::KeepAspectRatio, Qt::SmoothTransformation);

	// Finally paint
	painter->drawImage(getStrokeRect(index).topLeft(), strokeImage);
}

QRectF OilBrushElement::getStrokeRect(int index) const
{
	BrushStroke stroke = strokes.value(index);

	qreal size = getPaintSize(stroke.getSpeed());

	QRectF strokeRect(stroke, QSizeF(size, size));

	return strokeRect.translated(-size / 2, -size / 2);
}

}