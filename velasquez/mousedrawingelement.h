#ifndef MOUSEDRAWINGELEMENT_H
#define MOUSEDRAWINGELEMENT_H

#include "drawingelement.h"

namespace Velasquez
{

// Basic mouse drawing element class
class MouseDrawingElement : public DrawingElement
{
public:
	enum Settings {PenColor, PenTransparency, PenWidth};

public:
	MouseDrawingElement(QGraphicsItem *parent = 0);

	virtual QRectF boundingRect() const;

	virtual void setSetting(qint32 id, const QVariant &value);
	virtual QVariant getSetting(qint32 id) const;
	virtual QList <qint32> getSettingsList() const;
	virtual bool isSettingReversible(qint32 id) const;

	// Drawing process
	virtual void startDrawing();
	virtual void addPoint(const QPointF &point) = 0;
	virtual void finishDrawing();

protected:
	// Current color
	QColor currentColor;

	// Current alpha
	qreal currentAlpha;

	// Current width
	qreal currentWidth;

	// Bounding rect
	QRectF rect;

	// Update total rect after adding of new point
	virtual void updateRect(const QRectF &pointRect);

	// Mouse drawing element settings
	virtual void setColor(const QColor &color);
	inline virtual bool hasColor() const { return true; };

	virtual void setAlpha(qreal alpha);
	inline virtual bool hasAlpha() const { return true; };

	virtual void setWidth(qreal width);
	inline virtual bool hasWidth() const { return true; };

	// Drawing process control
	inline bool isDrawingNow() const { return drawingNow; };

private:
	bool drawingNow;
};

}

#endif // MOUSEDRAWINGELEMENT_H
