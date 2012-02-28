#ifndef SIMPLEBRUSHELEMENT_H
#define SIMPLEBRUSHELEMENT_H

#include "brushdrawingelement.h"

namespace Velasquez
{

#define DEFAULT_SIMPLEBRUSH_COLOR		0xFF0000

#define SIMPLEBRUSH_INDEX_COUNT			7
#define SIMPLEBRUSH_DEFAULT_INDEX		3

// Widths
const qreal SimpleBrushWidths[SIMPLEBRUSH_INDEX_COUNT] = {1, 2, 4, 8, 12, 16, 20};

// Spacings
const qreal SimpleBrushSpacings[SIMPLEBRUSH_INDEX_COUNT] = {2, 4, 4, 4, 4, 4, 4};

// Transparencies
const qreal SimpleBrushTransparencies[SIMPLEBRUSH_INDEX_COUNT] = {0.05, 0.10, 0.20, 0.35, 0.50, 0.70, 1};

// Very simple brush
class SimpleBrushElement : public BrushDrawingElement
{
public:
	// Settings and constants
	enum {Type = 10007};

public:
	SimpleBrushElement(QGraphicsItem *parent = 0);

	virtual inline int getPollingInterval() const { return 0; };

protected:
	virtual qint32 getType() const;

	inline virtual qreal getSpacing() const { return currentSpacing; };
	inline virtual bool isBuildup() const { return false; };

	virtual void setAlpha(qreal alpha);
	virtual void setWidth(qreal width);
	
	virtual void configureStrokesPainter(QPainter *painter);
	virtual void paintStroke(QPainter *painter, int index);

	virtual void mergePaintStroke(QPainter *painter, const QImage &strokeImage, const QRectF &strokeRect);
	
	virtual QRectF getStrokeRect(int index) const;
	QRectF getSingleStrokeRect(int index) const;

private:
	qreal currentSpacing;
};

}

#endif // SIMPLEBRUSHELEMENT_H
