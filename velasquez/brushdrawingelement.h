#ifndef BRUSHELEMENT_H
#define BRUSHELEMENT_H

#include "mousedrawingelement.h"

#include "brushstroke.h"

namespace Velasquez
{

// Time between readings, in msec
#define DEFAULT_BRUSH_POLLING_TIME		20

// Base class for all brushes
class BrushDrawingElement : public MouseDrawingElement
{
public:
	BrushDrawingElement(QGraphicsItem *parent = 0);

	virtual bool isEmpty() const;

	virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget = 0);
	virtual QRectF boundingRect() const;

	virtual void startDrawing();
	virtual void addPoint(const QPointF &point);
	virtual void finishDrawing();

	// If <= 0 then no polling
	virtual inline int getPollingInterval() const { return DEFAULT_BRUSH_POLLING_TIME; };

protected:
	enum CacheMode { NotInitialized, Drawing, Updating };
	enum InterpolateMode { Exact, Excessive };

	// Caching
	QRectF drawingRect;
	QImage cacheImage;
	QPainter *cachePainter;

	virtual void initCache(CacheMode mode);
	virtual void paintToCache(const QRectF &rect);
	virtual void doneCache();
	inline CacheMode getCacheMode() const { return cacheMode; };

	void updateCache();

	// Map element coordinates to cache image
	QRectF mapToCache(const QRectF &elementRect) const;

	// Interpolate functions
	// Create stroke(s) from new point
	virtual bool interpolate(const QPointF &newPoint);
	inline void setInterpolateMode(InterpolateMode mode) { interpolateMode = mode; };
	inline InterpolateMode getInterpolateMode () const { return interpolateMode; };

	// Interpolation parameters: spacing, filtering of points, measuring of angle and speed
	inline virtual qreal getSpacing() const { return 0; };
	inline virtual bool isBuildup() const { return false; };
	inline virtual bool isTrackingSpeed() const { return false; };
	inline virtual bool isTrackingAngle() const { return false; };

	// Stroke functions
	// Paint the stroke
	virtual void paintStroke(QPainter *painter, int index) = 0;
	virtual void configureStrokesPainter(QPainter *painter);

	// Get the stroke rect
	virtual QRectF getStrokeRect(int index) const = 0;

	// Rect functions
	// Update total rect after adding of new point
	virtual void updateRect(const QRectF &pointRect);

	// Update total rect from path
	virtual QRectF getRectFromPath(int start = 0, int finish = -1) const;

	// Check for the stroke to be inside the rect
	bool isStrokeInRect(int index, const QRectF &rect) const;

protected:
	QList <BrushStroke> strokes;

private:
	qreal interpolateRemainder;
	
	CacheMode cacheMode;
	InterpolateMode interpolateMode;
};

}

#endif // BRUSHELEMENT_H
