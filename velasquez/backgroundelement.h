#ifndef BACKGROUNDELEMENT_H
#define BACKGROUNDELEMENT_H

#include "drawingelement.h"

namespace Velasquez
{

// Scene background element
class BackgroundElement : public DrawingElement
{
public:
	// Settings and constants
	enum {Type = 10008};

public:	
	BackgroundElement(QGraphicsItem *parent = 0);

	virtual bool isEmpty() const { return false; };

	virtual void setSetting(qint32 id, const QVariant &value);
	virtual QVariant getSetting(qint32 id) const;
	virtual QList <qint32> getSettingsList() const;

	virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget = 0);
	virtual QRectF boundingRect() const;
	
	virtual void setColor(const QColor &color);
	inline QColor getColor() const { return currentColor; };

protected:
	virtual QVariant itemChange(GraphicsItemChange change, const QVariant &value);

	virtual qint32 getType() const;

private:
	QColor currentColor;

	void setSceneColor(QGraphicsScene *scene);
};

}

#endif // BACKGROUNDELEMENT_H