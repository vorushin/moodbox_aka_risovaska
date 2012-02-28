#include "simplebrushelement.h"

#include <QPainter>
#include <math.h>

#include "debug.h"

namespace Velasquez
{

SimpleBrushElement::SimpleBrushElement(QGraphicsItem *parent) 
	: BrushDrawingElement(parent)
{
	currentColor = DEFAULT_SIMPLEBRUSH_COLOR;
	currentWidth = SimpleBrushWidths[SIMPLEBRUSH_DEFAULT_INDEX];
	currentSpacing = SimpleBrushSpacings[SIMPLEBRUSH_DEFAULT_INDEX];
	currentAlpha = currentColor.alphaF();
}

qint32 SimpleBrushElement::getType() const
{
	return Type;
}

void SimpleBrushElement::setAlpha(qreal alpha)
{
	int pos = qMin((int)alpha, SIMPLEBRUSH_INDEX_COUNT - 1);
		
	alpha = SimpleBrushTransparencies[pos];

	BrushDrawingElement::setAlpha(alpha);
}

void SimpleBrushElement::setWidth(qreal width)
{
	int pos = qMin((int)width, SIMPLEBRUSH_INDEX_COUNT) - 1;

	width = SimpleBrushWidths[pos];
	currentSpacing = SimpleBrushSpacings[pos];

	BrushDrawingElement::setWidth(width);
}

void SimpleBrushElement::configureStrokesPainter(QPainter *painter)
{
	BrushDrawingElement::configureStrokesPainter(painter);

	QColor paintColor = currentColor;
	paintColor.setAlphaF(currentAlpha);
	
	QPen pen;
	pen.setColor(paintColor);	
	pen.setWidthF(currentWidth);

	pen.setCapStyle(Qt::RoundCap);
	painter->setPen(pen);
}

void SimpleBrushElement::paintStroke(QPainter *painter, int index)
{	
	QRectF strokeRect = getStrokeRect(index);

	QImage strokeImage = QImage(strokeRect.width(), strokeRect.height(), QImage::Format_ARGB32_Premultiplied);
	strokeImage.fill(qRgba(0, 0, 0, 0));

	QPainter strokePainter(&strokeImage);
	strokePainter.setRenderHint(QPainter::Antialiasing);
	strokePainter.setPen(painter->pen());	

	strokePainter.translate(-strokeRect.left(), -strokeRect.top());

	if (index == 0)
	{
		strokePainter.drawPoint(strokes.value(index));
	}
	else
	{
		strokePainter.drawLine(strokes.value(index), strokes.value(index - 1));
	}

	mergePaintStroke(painter, strokeImage, strokeRect);	
}

void SimpleBrushElement::mergePaintStroke(QPainter *painter, const QImage &strokeImage, const QRectF &strokeRect)
{
	QTransform painterTransform = painter->transform();
	QRect cacheImageRect(0, 0, cacheImage.width(), cacheImage.height());

	for (int y = 0; y < strokeImage.height(); y++)
	{	
		for (int x = 0; x < strokeImage.width(); x++)
		{						
			QPoint cacheImagePoint = painterTransform.map(QPointF(x + strokeRect.left(), y + strokeRect.top())).toPoint();
			
			if (!cacheImageRect.contains(cacheImagePoint))
				continue;

			QRgb cacheColor = cacheImage.pixel(cacheImagePoint);
			QRgb strokeColor = strokeImage.pixel(x, y);

			QRgb max = qMax(cacheColor, strokeColor);

			cacheImage.setPixel(cacheImagePoint, max);
		}
	}
}

QRectF SimpleBrushElement::getStrokeRect(int index) const
{
	QRectF thisRect = getSingleStrokeRect(index);

	if (index == 0)
		return thisRect;

	QRectF previousRect = getSingleStrokeRect(index - 1);

	QRectF strokeRect = thisRect.unite(previousRect);
	
	return strokeRect;
}

QRectF SimpleBrushElement::getSingleStrokeRect(int index) const
{
	BrushStroke stroke = strokes.value(index);

	qreal maxWidth = ceil(currentWidth / 2.0) ;

	QRectF strokeRect(stroke, QSizeF(maxWidth * 2, maxWidth * 2));

	return strokeRect.translated(-maxWidth, -maxWidth);	
}

}