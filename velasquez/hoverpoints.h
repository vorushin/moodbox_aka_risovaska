#ifndef HOVERPOINTS_H
#define HOVERPOINTS_H

#include <QObject>
#include <QGraphicsItem>
#include <QFlags>

#include "transformation.h"

namespace Velasquez
{

// All hover points have very high z-value to stay on the top of other items
#define HOVER_POINTS_ZVALUE						1000000

// Length of remove hover point side, in pixels 
#define REMOVE_HOVER_POINT_SIZE					12
#define RESIZE_ROTATE_HOVER_POINT_SIZE			12
#define REFLECT_HOVER_POINT_SIZE				12
#define RESIZE_ROTATE_CORNER_SIZE_PERCENTS		30
#define MENU_HOVER_POINT_SIZE					12

// Hover points tooltips
#define REMOVE_HOVER_POINT_TOOLTIP		QT_TRANSLATE_NOOP("Velasquez::RemoveHoverPoint", "RemoveHint")
#define REFLECT_HOVER_POINT_TOOLTIP		QT_TRANSLATE_NOOP("Velasquez::ReflectHoverPoint", "ReflectHint")
#define MENU_HOVER_POINT_TOOLTIP		QT_TRANSLATE_NOOP("Velasquez::MenuHoverPoint", "MenuHint")

// Angle to "stick" when rotating in sticky mode (degrees)
#define STICKY_ROTATION_ANGLE			90

// Minimal distance to ignore when rotating in sticky mode (pixels)
#define STICKY_ROTATION_DISTANCE		10

class TransformableElement;

// Hover point base class
class HoverPoint : public QObject, public QGraphicsItem
{
	Q_OBJECT

public:
	HoverPoint(TransformableElement *parentElement);

signals:
	void pressed(const QPointF &pos = QPointF());
	void released(const QPointF &pos = QPointF());

public slots:
	virtual void updateAnchor();

protected:
	virtual void mousePressEvent(QGraphicsSceneMouseEvent *event);
	virtual void mouseReleaseEvent(QGraphicsSceneMouseEvent *event);

	virtual bool isActive() const;

	virtual QPointF getAnchor() const = 0;
	virtual QPolygonF getParentBounds() const;

protected:
	TransformableElement *parentElement;

	bool isUnderMousePress() const { return underMousePress; };

private:
	bool underMousePress;	
};

// Class for remove element hover point
class RemoveHoverPoint : public HoverPoint
{
	Q_OBJECT

public:
	enum { Type = 100001 };

public:
	RemoveHoverPoint(TransformableElement *parentElement);

	virtual QRectF boundingRect() const;

    virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget = 0);

	inline virtual int type() const { return Type; };
   
protected:
	virtual QPointF getAnchor() const;
};

// Class for resize and rotation hovers
class ResizeRotateHoverPoint : public HoverPoint
{
	Q_OBJECT

public:
	enum LocationFlag { Left = 0x01, Right = 0x02, Top = 0x04, Bottom = 0x08 };
	Q_DECLARE_FLAGS(Location, LocationFlag)

	enum { Type = 100002 };

public:
	ResizeRotateHoverPoint(TransformableElement *parentElement, Location location);

	virtual QRectF boundingRect() const;
    virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget = 0 );

	inline virtual int type() const { return Type; };
	inline Location getLocation() const { return location; };
   
signals:
	void resizeRotate(qreal diffScale, qreal diffAngle);

public slots:
	virtual void updateAnchor();

protected:
	virtual void mousePressEvent(QGraphicsSceneMouseEvent *event);
	virtual void mouseMoveEvent(QGraphicsSceneMouseEvent *event);
	virtual void mouseReleaseEvent(QGraphicsSceneMouseEvent *event);

	virtual QPointF getAnchor() const;

	// This method analyzes transformation of parent element to return the correct location of hover
	virtual Location getLocationAfterTransform() const;

	// Evaluates scale and angle change
	void changeScaleAndAngle(const QPointF &newPos);

	// Checks rotation angle and sticks it if necessary
	qreal getStickyRotationAngle(qreal diffAngle, qreal radius) const;

	// Checks scale and makes sure it will work
	bool isSafeScale(qreal diffScale) const;

private:
	QPointF startPos, anchorCorrection;
	Location location;
	qreal stickyAccumulatedAngle;

	bool inStickyMode;
};

Q_DECLARE_OPERATORS_FOR_FLAGS(ResizeRotateHoverPoint::Location)

// Class for reflecton hovers
class ReflectHoverPoint : public HoverPoint
{
	Q_OBJECT
public:
	enum { Type = 100003 };

public:
	ReflectHoverPoint(TransformableElement *parentElement);

	virtual QRectF boundingRect() const;
	virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget = 0 );

	virtual int type() const {return Type;};

signals:
	void reflected(bool newReflected);
   
protected:
	virtual QPointF getAnchor() const;
};

// ResizeRotateCorner class
class ResizeRotateCorner : public ResizeRotateHoverPoint
{
	Q_OBJECT

public:
	enum { Type = 100004 };

public:
	ResizeRotateCorner(TransformableElement *parentElement, Location location);

	virtual QRectF boundingRect() const;
    virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget = 0);
	QPainterPath shape() const;

	inline virtual int type() const { return Type; };
  
public slots:
	virtual void updateAnchor();

protected:
	virtual QPointF getAnchor() const;
	virtual Location getLocationAfterTransform() const;

private:
	enum GetShapePathMode { Bounds, Drawing };
	qreal side;

	bool updateSide();
	QPainterPath getShapePath(const QRectF &rect, GetShapePathMode mode) const;
};

// Class for context menu hover point
class MenuHoverPoint : public HoverPoint
{
	Q_OBJECT

public:
	enum { Type = 100005 };

public:
	MenuHoverPoint(TransformableElement *parentElement);

	virtual QRectF boundingRect() const;

    virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget = 0);

	inline virtual int type() const { return Type; };

protected:
	virtual QPointF getAnchor() const;

};

}

#endif // HOVERPOINTS_H