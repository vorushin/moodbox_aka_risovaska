#ifndef OILBRUSHELEMENT_H
#define OILBRUSHELEMENT_H

#include "brushdrawingelement.h"

namespace Velasquez
{

#define DEFAULT_OILBRUSH_COLOR			0xFF0000
#define DEFAULT_OILBRUSH_WIDTH			12.0

#define OILBRUSH_IMAGE					":/Velasquez/Resources/OilBrush.png"

#define OILBRUSH_SPACING				2.0

// Width scale
#define OILBRUSH_WIDTH_SCALE			6.0

// Color deviation settings
#define OILBRUSH_COLORDEV_HUE_RANGE		0.02
#define OILBRUSH_COLORDEV_SAT_RANGE		0.1
#define OILBRUSH_COLORDEV_VAL_RANGE		0.05

// Transparencies
#define OILBRUSH_TRANSPARENCY_COUNT		7

const qreal OilBrushTransparencies[OILBRUSH_TRANSPARENCY_COUNT] = {0.10, 0.20, 0.30, 0.45, 0.60, 0.80, 1};

// Oil brush painting element
class OilBrushElement : public BrushDrawingElement
{
public:
	// Settings and constants
	enum {Type = 10004};

public:
	OilBrushElement(QGraphicsItem *parent = 0);
	
protected:
	QImage alphaImage;

	virtual qint32 getType() const;

	inline virtual qreal getSpacing() const { return OILBRUSH_SPACING; };
	inline virtual bool isTrackingSpeed() const { return true; };
	inline virtual bool isTrackingAngle() const { return true; };

	virtual void setColor(const QColor &color);
	virtual void setAlpha(qreal alpha);
	virtual void setWidth(qreal width);

	virtual qreal getPaintTransparency(qreal speed) const;
	virtual qreal getPaintSize(qreal speed) const;

	virtual void updateAlphaImage();

	virtual void configureStrokesPainter(QPainter *painter);
	virtual void paintStroke(QPainter *painter, int index);

	virtual QRectF getStrokeRect(int index) const;
};

}

#endif // OILBRUSHELEMENT_H
