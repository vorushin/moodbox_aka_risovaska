#ifndef ERASERELEMENT_H
#define ERASERELEMENT_H

#include "brushdrawingelement.h"

namespace Velasquez
{

#define ERASER_SPACING				2.0
#define DEFAULT_ERASER_WIDTH		2.0

#define	ERASER_WIDTH_SCALE			4.0

class EraserElement : public BrushDrawingElement
{
public:
	// Settings and constants
	enum {Type = 10005};

public:	
	EraserElement(QGraphicsItem *parent = 0);

	virtual void setSetting(qint32 id, const QVariant &value);
	virtual QVariant getSetting(qint32 id) const;
	virtual QList <qint32> getSettingsList() const;

	virtual void sceneEvent(qint32 id, const QVariant &data);

protected:
	virtual QVariant itemChange(GraphicsItemChange change, const QVariant &value);

	virtual qint32 getType() const;

	inline virtual qreal getSpacing() const { return ERASER_SPACING; };
	inline virtual bool isBuildup() const { return false; };

	inline virtual bool hasAlpha() const { return false; };
	inline virtual bool hasColor() const { return false; };

	virtual void setWidth(qreal width);
	
	virtual void configureStrokesPainter(QPainter *painter);
	virtual void paintStroke(QPainter *painter, int index);

	virtual QRectF getStrokeRect(int index) const;
};

}

#endif // ERASERELEMENT_H
