#include "sprayelement.h"

#include <QPainter>

#include "debug.h"
#include "drawingutils.h"

namespace Velasquez
{

SprayElement::SprayElement(QGraphicsItem *parent) 
	: BrushDrawingElement(parent)
{
	currentColor = DEFAULT_SPRAY_COLOR;
	currentWidth = DEFAULT_SPRAY_WIDTH;
	currentAlpha = currentColor.alphaF();

	spacing = SPRAY_SPACING;
}

qint32 SprayElement::getType() const
{
	return Type;
}

void SprayElement::setColor(const QColor &color)
{
	QColor newColor = color;

	newColor = DrawingUtils::addRandomColorDeviation(color, SPRAY_COLORDEV_HUE_RANGE, SPRAY_COLORDEV_SAT_RANGE, SPRAY_COLORDEV_VAL_RANGE);

	BrushDrawingElement::setColor(newColor);
}

void SprayElement::setAlpha(qreal alpha)
{
	int pos = qMin((int)alpha, SPRAY_TRANSPARENCY_COUNT - 1);
		
	alpha = SprayTransparencies[pos];

	BrushDrawingElement::setAlpha(alpha);
}

void SprayElement::setWidth(qreal width)
{
	width *= SPRAY_WIDTH_SCALE;

	qreal scale = width / currentWidth;
	spacing = scale * spacing;

	BrushDrawingElement::setWidth(width);
	updateAlphaImage();
}

void SprayElement::updateAlphaImage()
{
	alphaImage = QImage(SPRAY_IMAGE).scaled(currentWidth * SPRAY_SIZE_SCALE, currentWidth * SPRAY_SIZE_SCALE, Qt::KeepAspectRatio, Qt::SmoothTransformation).alphaChannel();
}

void SprayElement::configureStrokesPainter(QPainter *painter)
{
	BrushDrawingElement::configureStrokesPainter(painter);

	updateAlphaImage();
}

void SprayElement::paintStroke(QPainter *painter, int index)
{
	BrushStroke stroke = strokes.value(index);

	// Set alpha
	QColor imageColor = currentColor;
	imageColor.setAlphaF(currentAlpha);

	// Create image
	QImage strokeImage = QImage(alphaImage.width(), alphaImage.height(), QImage::Format_ARGB32);
	strokeImage.fill(imageColor.rgba());

	// Apply alpha
	strokeImage.setAlphaChannel(alphaImage);

	// Apply scale and transformation
	QTransform strokeTransform;
	qreal scale = DrawingUtils::rangedRandomF(0.8, 1.0);

	strokeTransform.rotate(DrawingUtils::rangedRandom(0, 360));
	strokeTransform.scale(scale, scale);	

	strokeImage = strokeImage.transformed(strokeTransform);

	// Calc exact center of stroke considering stroke image transform
	QPointF drawPos = stroke;
	drawPos.rx() -= strokeImage.width() / 2;
	drawPos.ry() -= strokeImage.height() / 2;

	// Finally paint
	painter->drawImage(drawPos, strokeImage);
}

QRectF SprayElement::getStrokeRect(int index) const
{
	BrushStroke stroke = strokes.value(index);

	QRectF strokeRect(stroke, QSizeF(alphaImage.width(), alphaImage.height()));

	return strokeRect.translated(-strokeRect.width() / 2, -strokeRect.height() / 2);
}

}