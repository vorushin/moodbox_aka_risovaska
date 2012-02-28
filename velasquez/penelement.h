#ifndef PENELEMENT_H
#define PENELEMENT_H

#include "brushdrawingelement.h"

namespace Velasquez
{

#define DEFAULT_PEN_COLOR			0xFF0000
#define DEFAULT_PEN_WIDTH			3.0

#define PEN_IMAGE					":/Velasquez/Resources/Pen.png"
#define PEN_SPACING					8.0

// Transparencies
#define PEN_TRANSPARENCY_COUNT		7

const qreal PenTransparencies[PEN_TRANSPARENCY_COUNT] = {0.15, 0.22, 0.30, 0.45, 0.60, 0.80, 1};
	
// Pen painting element
class PenElement : public BrushDrawingElement
{
public:
	// Settings and constants
	enum {Type = 10000};

public:
	PenElement(QGraphicsItem *parent = 0);
	
protected:
	virtual qint32 getType() const;

	inline virtual qreal getSpacing() const { return spacing; };
	inline virtual bool isTrackingSpeed() const { return true; };
	inline virtual bool isTrackingAngle() const { return true; };

	virtual qreal getPaintTransparency(qreal speed) const;
	virtual qreal getPaintAngle(qreal angle) const;
		
	virtual void setAlpha(qreal alpha);
	virtual void setWidth(qreal width);

	virtual void paintStroke(QPainter *painter, int index);
		
	virtual QRectF getStrokeRect(int index) const;

private:
	qreal spacing;

};

}

#endif // PENELEMENT_H
