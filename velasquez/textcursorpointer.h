#ifndef TEXTCURSORPOINTER_H
#define TEXTCURSORPOINTER_H

#include <QGraphicsItem>
#include <QObject>
#include <QList>
#include <QLineF>

namespace Velasquez
{

#define CURSOR_POINTER_WIDTH		22
#define CURSOR_POINTER_HEIGHT		7

// Cursor layer item
#define CURSOR_POINTER_ZVALUE		10000000

class TextElement;

// Pointer to cursor out of scene rect
class TextCursorPointer : public QObject, public QGraphicsItem
{
	Q_OBJECT

public:
	enum { Type = 20001 };

public:
	TextCursorPointer(QGraphicsItem *parentItem = 0);
	
	virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget = 0);
	virtual QRectF boundingRect() const;

	virtual int type () const { return Type; };
	inline bool isActive() const { return active; };

public slots:
	void setActive(bool active);

	// Starts pointing to cursor withing selected item cursor pos
	virtual void pointToCursor(TextElement *element);

protected:
	virtual QVariant itemChange(GraphicsItemChange change, const QVariant &value);

	virtual void startBlinking();
	virtual void stopBlinking();

	void timerEvent(QTimerEvent *event);

private:
	bool active;
	int timerId;

	QRectF sceneRect;
	QList <QLineF> sceneBounds;

	// Checks for intersection with scene bound, return bound number or -1
	int getSceneBoundIntersection(const QLineF &cursorMidLine, QPointF &newPos, qreal &newAngle) const;
	
	// Get non-intersected pos and angle
	int getNotIntersectedUpdate(const QLineF &cursorMidLine, QPointF &newPos, qreal &newAngle, int &crossBoundIndex) const;

	// Get updated pos and angle
	void getUpdate(const QPolygonF &cursorPolygon, QPointF &newPos, qreal &newAngle) const;	

private slots:
	void onSceneRectChanged(const QRectF &rect);

};

}
#endif // TEXTCURSORPOINTER_H
