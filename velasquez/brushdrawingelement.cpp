#include "brushdrawingelement.h"

#include <QStyleOptionGraphicsItem>
#include <QPainter>
#include <QLineF>
#include <math.h>

#include "editorscene.h"
#include "drawingutils.h"
#include "vcommon.h"

#include "debug.h"

namespace Velasquez
{

// BrushDrawingElement class
BrushDrawingElement::BrushDrawingElement(QGraphicsItem *parent)
	: MouseDrawingElement(parent), cachePainter(NULL), interpolateRemainder(0), cacheMode(NotInitialized), interpolateMode(Exact)
{
}

bool BrushDrawingElement::isEmpty() const
{
	return strokes.isEmpty();
}

void BrushDrawingElement::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
	Q_UNUSED(widget)

	// Fill the cache first
	if (isDrawingNow())
	{
		paintToCache(option->exposedRect);
	}

	// Draw the exposed part
	painter->drawImage(option->exposedRect, cacheImage, mapToCache(option->exposedRect));
}

QRectF BrushDrawingElement::boundingRect() const
{
	return (isDrawingNow()) ? drawingRect : rect;
}

void BrushDrawingElement::startDrawing()
{
	MouseDrawingElement::startDrawing();

	drawingRect = scene()->sceneRect();
	drawingRect.moveTo(drawingRect.topLeft() - pos());

	initCache(Drawing);

	if (drawingRect != rect)
		prepareGeometryChange();
}

void BrushDrawingElement::addPoint(const QPointF &point)
{
	int i = strokes.count();

	if (!interpolate(point))
		return;

	// Add the rect of newly added
	QRectF newStrokesRect = getRectFromPath(i);

	updateRect(newStrokesRect);
	update(newStrokesRect);
}

void BrushDrawingElement::finishDrawing()
{
	MouseDrawingElement::finishDrawing();

	doneCache();

	if (drawingRect != rect)
		prepareGeometryChange();
}

void BrushDrawingElement::initCache(CacheMode mode)
{
	if (getCacheMode() != NotInitialized || mode == NotInitialized)
		return;

	cacheMode = mode;

	cacheImage = QImage(boundingRect().size().toSize(), QImage::Format_ARGB32_Premultiplied);
	cacheImage.fill(qRgba(0, 0, 0, 0));

	cachePainter = new QPainter(&cacheImage);
	configureStrokesPainter(cachePainter);

	// If we drawing on the big picture we draw from pos() otherwise we draw from the top left of rect
	cachePainter->translate( (mode == Drawing) ? pos() : -rect.topLeft());
}

void BrushDrawingElement::paintToCache(const QRectF &rect)
{
	for (int i = 0; i < strokes.size(); i++)
	{
		if (!isStrokeInRect(i, rect))
			continue;

		BrushStroke &stroke = strokes[i];
		
		if (!stroke.rendered)
		{
			paintStroke(cachePainter, i);
			stroke.rendered = true;
		}
	}
}

void BrushDrawingElement::doneCache()
{
	if (getCacheMode() == NotInitialized)
		return;

	cachePainter->end();
	
	// Make a tight picture if drawing
	if (getCacheMode() == Drawing)
	{		
		QRectF copyRectF = rect;
		copyRectF.moveTo(copyRectF.topLeft() + pos());

		QRect copyRect = copyRectF.toRect();
		cacheImage = cacheImage.copy(copyRect);
	}

	DELETE_AND_NULL(cachePainter);

	cacheMode = NotInitialized;
}

void BrushDrawingElement::updateCache()
{
	for (int i = 0; i < strokes.size(); i++)
		strokes[i].rendered = false;

	initCache(Updating);
	paintToCache(rect);
	doneCache();
}

QRectF BrushDrawingElement::mapToCache(const QRectF &elementRect) const
{
	QRectF cacheRect = elementRect;
	
	cacheRect.moveTo(elementRect.topLeft() - boundingRect().topLeft());
	return cacheRect;
}

bool BrushDrawingElement::interpolate(const QPointF &newPoint)
{
	BrushStroke newStroke = BrushStroke(newPoint);

	// Check for duplicates to avoid calculations
	if (!isEmpty())
	{
		if (strokes.last() == newPoint && !isBuildup())
			return false;
	}
	else
	{
		// The first stroke in the stack
		strokes.append(newStroke);
		return true;
	}

	BrushStroke lastStroke = strokes.last();
	QLineF newLine(lastStroke, newStroke);

	// Calculate speed, if needed
	if (isTrackingSpeed())
		newStroke.setSpeed(newLine.length() / getPollingInterval());

	// Calculate angle, if needed
	if (isTrackingAngle())
		newStroke.setAngle(-DrawingUtils::normalizeAngle(newLine.angle()));
	
	bool appended = false;

	if ( getSpacing() > 0 && !strokes.isEmpty() )
	{
		const qreal lineLen = newLine.length();

		// Check do we have enough space to draw something
		if (lineLen >= interpolateRemainder || lineLen >= getSpacing() || interpolateRemainder > getSpacing())
		{
			qreal startStep = getSpacing();

			// We start from the smallest part of interpolateRemainder
			if (interpolateRemainder > 0)
			{
				if (interpolateRemainder > getSpacing())
					// Use reminder if it is too big
					startStep = interpolateRemainder - qRound(interpolateRemainder / getSpacing());
				else
					// Or whole
					startStep = interpolateRemainder;
			}

			for (qreal p = startStep; p <= lineLen; p += getSpacing())
			{
				// Convert to 0..1 to line size
				qreal t = 1 / (lineLen / p);

				BrushStroke stroke(newLine.pointAt(t), true);
			
				if (isTrackingSpeed())
					stroke.setSpeed(lastStroke.getSpeed() * (1 - t) + newStroke.getSpeed() * t);

				if (isTrackingAngle())
				{
					if (getInterpolateMode() == Excessive)
					{
						stroke.setAngle(lastStroke.getAngle() * (1 - t) + newStroke.getAngle() * t);
					}
					else
						stroke.setAngle(newStroke.getAngle());
				}

				strokes.append(stroke);
				appended = true;

				// Decrease the remainder
				if (interpolateRemainder - p  >= 0)
					interpolateRemainder -= p;
			}
			
			// Make the last stroke real to use it in serialization
			if (appended)
				strokes.last().interpolated = false;
		}
	}
	else
	{
		strokes.append(newStroke);
		appended = true;
	}

	// Add new stroke if it was not added yet in excessive mode
	if (appended && getInterpolateMode() == Excessive)
	{
		if (strokes.last() != newStroke)
		{
			strokes.last().interpolated = false;
			strokes.append(newStroke);
		}
	}

	// Make sure we append in any case if Buildup is on
	if (!appended && isBuildup())
	{
		strokes.append(newStroke);
		appended = true;
	}

	return appended;
}

void BrushDrawingElement::configureStrokesPainter(QPainter *painter)
{
	painter->setRenderHint(QPainter::Antialiasing);
}

void BrushDrawingElement::updateRect(const QRectF &pointRect)
{
	QRectF newRect = rect.united(pointRect);	
	newRect = newRect.toRect();

	if (newRect == rect)
		return;
	
	if (!isDrawingNow())
		prepareGeometryChange();

	rect = newRect;
}

QRectF BrushDrawingElement::getRectFromPath(int start, int finish) const
{
	QRectF strokesRect = getStrokeRect(start);
	
	if (finish < start)
		finish = strokes.count() - 1;

	for (int i = start + 1; i <= finish; i++)
		strokesRect = strokesRect.united(getStrokeRect(i));
	
	return strokesRect.toAlignedRect();
}

bool BrushDrawingElement::isStrokeInRect(int index, const QRectF &rect) const
{
	QRectF strokeRect = getStrokeRect(index);

	return rect.intersects(strokeRect);
}

}