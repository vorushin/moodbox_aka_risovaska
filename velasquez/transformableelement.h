#ifndef TRANSFORMABLEELEMENT_H
#define TRANSFORMABLEELEMENT_H

#include <QObject>

#include "drawingelement.h"
#include "transformation.h"

namespace Velasquez
{

class HoverPoint;
class ResizeRotateHoverPoint;

#define ELEMENT_SELECTION_BORDER_COLOR1	0xFFFFFF
#define ELEMENT_SELECTION_BORDER_COLOR2	0x47B37D

#define REMOVE_MENU_ITEM_TEXT			QT_TRANSLATE_NOOP("Velasquez::TransformableElement", "RemoveMenuItem")
#define REFLECT_MENU_ITEM_TEXT			QT_TRANSLATE_NOOP("Velasquez::TransformableElement", "ReflectMenuItem")
#define BRING_FRONT_MENU_ITEM_TEXT		QT_TRANSLATE_NOOP("Velasquez::TransformableElement", "BringFrontMenuItem")
#define MOVE_FORWARD_MENU_ITEM_TEXT		QT_TRANSLATE_NOOP("Velasquez::TransformableElement", "MoveForwardMenuItem")
#define MOVE_BACKWARD_MENU_ITEM_TEXT	QT_TRANSLATE_NOOP("Velasquez::TransformableElement", "MoveBackwardMenuItem")
#define SEND_BACK_MENU_ITEM_TEXT		QT_TRANSLATE_NOOP("Velasquez::TransformableElement", "SendBackMenuItem")

// Transformable elements can change their position, scale and be rotated
// Usually they are complex elements containing hover points
class TransformableElement :  public QObject, public DrawingElement
{
	Q_OBJECT

public:
	TransformableElement(QGraphicsItem *parentItem = 0);
	virtual ~TransformableElement();

	virtual void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget = 0);

	// Transformation management
	virtual bool isValidTransformation(const Transformation &transformation, bool combine = false) const;
	virtual void setTransformation(const Transformation &transformation, bool combine = false);
	inline Transformation getTransformation() const { return transformation; };
	
	void setTransformationScale(qreal scale);
	void setTransformationAngle(qreal angle);
	void setTransformationReflected(bool reflected);
	
	virtual void transformToFit(const QSizeF &size, bool upsize = false);

	// Type information
	virtual QList <qint32> getDecorationTypes() const;

signals:
	// Emitted when this element is responsible for start or stop of a new changing session
	void changing(bool started);

	// Mouse events
	void mouseEnter(TransformableElement *element);
	void mouseMove(TransformableElement *element);
	void mouseLeave(TransformableElement *element);

	// Notification about movement
	void moved(TransformableElement *element, const QPointF &oldPos);

	// Notification about deletion
	void remove(DrawingElement *element);

	// Notification about resize and rotate
	void resizeRotate(TransformableElement *element, qreal diffScale, qreal diffAngle);

	// Notification about reflection
	void reflect(TransformableElement *element);

	// Notifications about Z-order change
	void bringFront(TransformableElement *element);
	void moveForward(TransformableElement *element);
	void moveBackward(TransformableElement *element);
	void sendBack(TransformableElement *element);

protected:
	virtual QVariant itemChange(GraphicsItemChange change, const QVariant &value);

	virtual void mousePressEvent(QGraphicsSceneMouseEvent *event);
	virtual void mouseReleaseEvent(QGraphicsSceneMouseEvent *event);

	// Hovers management
	virtual void hoverEnterEvent(QGraphicsSceneHoverEvent *event);
	virtual void hoverMoveEvent(QGraphicsSceneHoverEvent *event);
	virtual void hoverLeaveEvent(QGraphicsSceneHoverEvent *event);

	// Context menu management
	virtual QMenu *getContextMenu();
	bool isContextMenuActive() const { return contextMenuActive; };
	bool isHoverContextMenu() const { return hoverContextMenu; };

	// Changing session management
	virtual void startChanging();
	virtual void finishChanging();

	// In silent changing no signals emitted
	void setSilentChanging(bool on);
	inline bool isSilentChanging() const { return silentChanging; };

	// Moving notifications
	virtual void movingNotify(const QPointF &oldPos);
	
	// Utility functions
	bool isUnderLMousePress() const { return underLMousePress; };
	bool isHoverUnderMousePress() const { return hoverUnderMousePress; };

	// Frames management
	virtual void paintSelectionFrame(QPainter *painter);

	// Hover points management
	virtual void createHoverPoints();
	virtual void updateHoverPoints();
	virtual void destroyHoverPoints();

	// Hover points list for group operations
	void addHoverPointToList(HoverPoint *point);
	void removeHoverPointFromList(HoverPoint *point);

	void setupResizeRotateHoverPoint(ResizeRotateHoverPoint *point);

protected slots:
	// Hover events
	void onMenu(const QPointF &pos);

	inline void onReflect() { emit reflect(this); };
	inline void onRemove() { emit remove(this); };

	// Scale and angle change
	inline void onResizeRotateHoverPress() { hoverUnderMousePress = true; };
	void onResizeRotateHoverRelease();

	void onResizeRotate(qreal diffScale, qreal diffAngle);	

	// Z-order events
	inline void onBringFront() { emit bringFront(this); };
	inline void onMoveForward() { emit moveForward(this); };
	inline void onMoveBackward() { emit moveBackward(this); };
	inline void onSendBack() { emit sendBack(this); };

	// Context menu events
	void onContextMenuShow();
	void onContextMenuHide();

private:
	bool underLMousePress, hoverUnderMousePress;
	bool changingStarted;
	bool silentChanging;

	Transformation transformation;

	QList<HoverPoint *> hoverPointList;

	QMenu *contextMenu;
	bool contextMenuActive;
	bool hoverContextMenu;
};

}

#endif // TRANSFORMABLEELEMENT_H
