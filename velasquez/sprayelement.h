#ifndef SPRAYELEMENT_H
#define SPRAYELEMENT_H

#include "brushdrawingelement.h"

namespace Velasquez
{

#define DEFAULT_SPRAY_COLOR				0xFF0000
#define DEFAULT_SPRAY_WIDTH				30.0

#define SPRAY_SIZE_SCALE				2

#define SPRAY_IMAGE						":/Velasquez/Resources/Spray.png"

#define SPRAY_SPACING					10.0
#define	SPRAY_POLLING_TIME				60

// Width scale
#define SPRAY_WIDTH_SCALE				9.0

// Color deviation settings
#define SPRAY_COLORDEV_HUE_RANGE		0.02
#define SPRAY_COLORDEV_SAT_RANGE		0.1
#define SPRAY_COLORDEV_VAL_RANGE		0.05

// Transparencies
#define SPRAY_TRANSPARENCY_COUNT		7

const qreal SprayTransparencies[SPRAY_TRANSPARENCY_COUNT] = {0.10, 0.20, 0.30, 0.40, 0.60, 0.80, 1};

class SprayElement : public BrushDrawingElement
{
public:
	// Settings and constants
	enum {Type = 10006};

public:
	SprayElement(QGraphicsItem *parent = 0);

	virtual inline int getPollingInterval() const { return SPRAY_POLLING_TIME; };

protected:
	QImage alphaImage;

	virtual qint32 getType() const;

	inline virtual qreal getSpacing() const { return spacing; };
	inline virtual bool isBuildup() const { return true; };

	virtual void setColor(const QColor &color);
	virtual void setAlpha(qreal alpha);
	virtual void setWidth(qreal width);

	virtual void updateAlphaImage();
	
	virtual void configureStrokesPainter(QPainter *painter);
	virtual void paintStroke(QPainter *painter, int index);
	
	virtual QRectF getStrokeRect(int index) const;

private:
	qreal spacing;
};

}

#endif // SPRAYELEMENT_H
