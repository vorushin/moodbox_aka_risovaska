#include "transformableelement.h"

#include <QGraphicsScene>
#include <QGraphicsSceneEvent>
#include <QStyleOptionGraphicsItem>
#include <QPainter>
#include <QMenu>
#include <QGraphicsView>

#include "hoverpoints.h"

#include "vcommon.h"
#include "debug.h"

namespace Velasquez
{

TransformableElement::TransformableElement(QGraphicsItem *parentItem)
	: QObject(), DrawingElement(parentItem), underLMousePress(false), hoverUnderMousePress(false),
	changingStarted(false), silentChanging(false), contextMenu(NULL), contextMenuActive(false), hoverContextMenu(false)
{
	setAcceptHoverEvents(true);
	setFlags(QGraphicsItem::ItemIsMovable | QGraphicsItem::ItemIsSelectable);
}

TransformableElement::~TransformableElement()
{
	if (contextMenu != NULL)
		delete contextMenu;
}

void TransformableElement::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
	Q_UNUSED(widget)
	
	if (option->state & QStyle::State_Selected)
		paintSelectionFrame(painter);
}

bool TransformableElement::isValidTransformation(const Transformation &transformation, bool combine) const
{
	qreal newSide = qMax(boundingRect().width(), boundingRect().height());
	
	Transformation newTransformation = transformation;

	if (combine)
		newTransformation += getTransformation();

	newSide *= newTransformation.getScale();

	return (newSide >= MENU_HOVER_POINT_SIZE * 1.2 && newTransformation.getScale() < 1000000 && newTransformation.getScale() > 0.000000001);
}

void TransformableElement::setTransformation(const Transformation &transformation, bool combine)
{
	Transformation newTransformation = this->transformation;

	if (combine)
		newTransformation += transformation;
	else
		newTransformation = transformation;

	if (this->transformation == newTransformation)
		return;

	if (!isValidTransformation(newTransformation))
		return;

	this->transformation = newTransformation;
	setTransform(this->transformation.getMatrix(this));
}

void TransformableElement::setTransformationScale(qreal scale)
{
	Transformation t = this->transformation;
	t.setScale(scale);

	setTransformation(t);
}

void TransformableElement::setTransformationAngle(qreal angle)
{
	Transformation t = this->transformation;
	t.setAngle(angle);

	setTransformation(t);
}

void TransformableElement::setTransformationReflected(bool reflected)
{
	Transformation t = this->transformation;
	t.setReflected(reflected);

	setTransformation(t);
}

void TransformableElement::transformToFit(const QSizeF &size, bool upsize)
{
	if (isEmpty())
		return;
	
	QSizeF itemSize = boundingRect().size();
	
	if (upsize || itemSize.width() > size.width() || itemSize.height() > size.height())
	{
		qreal maxSize = qMax(itemSize.width(), itemSize.height());

		if (maxSize == 0)
			return;

		qreal diffScale = qMax(size.width(), size.height()) / maxSize;
		
		setTransformationScale(diffScale);
	}
}

QList <qint32> TransformableElement::getDecorationTypes() const
{
	QList <qint32> ignoreTypes;

	foreach (HoverPoint *hoverPoint, hoverPointList)
	{
		if (!ignoreTypes.contains(hoverPoint->type()))
			ignoreTypes << hoverPoint->type();
	}

	return ignoreTypes;
}
	
QVariant TransformableElement::itemChange(GraphicsItemChange change, const QVariant &value)
{
	if (change == ItemSceneChange)
	{
		if (hoverPointList.isEmpty())
			createHoverPoints();
	}

    if (change == ItemPositionChange && scene() != NULL)
	{
 		startChanging();
		movingNotify(pos());
    }

	if (change == ItemSelectedHasChanged || change == ItemTransformHasChanged)
		updateHoverPoints();

	return DrawingElement::itemChange(change, value);
}

void TransformableElement::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
	if (event->button() == Qt::LeftButton)
	{
		underLMousePress = true;
	}

	DrawingElement::mousePressEvent(event);
}

void TransformableElement::mouseReleaseEvent(QGraphicsSceneMouseEvent *event)
{
	if (event->button() == Qt::LeftButton)
	{
		underLMousePress = false;

		finishChanging();
	}

	DrawingElement::mouseReleaseEvent(event);
}

void TransformableElement::hoverEnterEvent(QGraphicsSceneHoverEvent *event)
{
	DrawingElement::hoverEnterEvent(event);

	emit mouseEnter(this);	
}

void TransformableElement::hoverMoveEvent(QGraphicsSceneHoverEvent *event)
{
	DrawingElement::hoverMoveEvent(event);

	emit mouseMove(this);
}

void TransformableElement::hoverLeaveEvent(QGraphicsSceneHoverEvent *event)
{
	DrawingElement::hoverLeaveEvent(event);

	if (!isContextMenuActive())
		emit mouseLeave(this);
}

QMenu *TransformableElement::getContextMenu()
{
	if (contextMenu == NULL)
	{
		contextMenu = new QMenu();

		// Reflect
		QAction *action = contextMenu->addAction(tr(REFLECT_MENU_ITEM_TEXT));
		connect(action, SIGNAL(triggered()), this, SLOT(onReflect()));
		
		contextMenu->addSeparator();

		// Bring front
		action = contextMenu->addAction(tr(BRING_FRONT_MENU_ITEM_TEXT));
		connect(action, SIGNAL(triggered()), this, SLOT(onBringFront()));

		// Move forward
		action = contextMenu->addAction(tr(MOVE_FORWARD_MENU_ITEM_TEXT));
		connect(action, SIGNAL(triggered()), this, SLOT(onMoveForward()));

		// Move backward
		action = contextMenu->addAction(tr(MOVE_BACKWARD_MENU_ITEM_TEXT));
		connect(action, SIGNAL(triggered()), this, SLOT(onMoveBackward()));

		// Send back
		action = contextMenu->addAction(tr(SEND_BACK_MENU_ITEM_TEXT));
		connect(action, SIGNAL(triggered()), this, SLOT(onSendBack()));

		contextMenu->addSeparator();

		// Remove
		action = contextMenu->addAction(tr(REMOVE_MENU_ITEM_TEXT));
		connect(action, SIGNAL(triggered()), this, SLOT(onRemove()));

		connect(contextMenu, SIGNAL(aboutToShow()), this, SLOT(onContextMenuShow()));
		connect(contextMenu, SIGNAL(aboutToHide()), this, SLOT(onContextMenuHide()));
	}

	return contextMenu;
}

void TransformableElement::startChanging()
{
	// Element can change if it is under mouse press or any of its hovers are
	if ( (isUnderLMousePress() || isHoverUnderMousePress() ) && !changingStarted)
	{
		changingStarted = true;

		if (isSilentChanging())
			return;

		emit changing(true);
	}
}

void TransformableElement::finishChanging()
{
	if (changingStarted)
	{
		changingStarted = false;

		if (isSilentChanging())
			return;

		emit changing(false);
	}
}

void TransformableElement::setSilentChanging(bool on)
{
	silentChanging = on;
}

void TransformableElement::movingNotify(const QPointF &oldPos)
{	
	if (isSilentChanging())
		return;

	emit moved(this, oldPos);
}

void TransformableElement::paintSelectionFrame(QPainter *painter)
{
	painter->save();
	
	painter->setRenderHint(QPainter::Antialiasing, true);
	
	// Outer color
	QColor color(ELEMENT_SELECTION_BORDER_COLOR1);

	QPen pen(color);
	pen.setWidthF(1.0 / this->transformation.getScale());

	painter->setPen(pen);
	qreal penWidth = pen.widthF() / 2.0;

	painter->drawRect(boundingRect().adjusted(penWidth, penWidth, -penWidth, -penWidth));

	// Inner color
	color = QColor(ELEMENT_SELECTION_BORDER_COLOR2);
	pen.setColor(color);

	painter->setPen(pen);
	penWidth += pen.widthF();

	painter->drawRect(boundingRect().adjusted(penWidth, penWidth, -penWidth, -penWidth));

	painter->restore();
}

void TransformableElement::createHoverPoints()
{
	// Menu hover point
	MenuHoverPoint *menuHover = new MenuHoverPoint(this);
	connect(menuHover, SIGNAL(released(const QPointF &)), this, SLOT(onMenu(const QPointF &)));
	addHoverPointToList(menuHover);

	// Resize-rotate hovers
	ResizeRotateHoverPoint *rrHover = new ResizeRotateCorner(this, ResizeRotateHoverPoint::Left | ResizeRotateHoverPoint::Top);
	setupResizeRotateHoverPoint(rrHover);

	rrHover = new ResizeRotateCorner(this, ResizeRotateHoverPoint::Left | ResizeRotateHoverPoint::Bottom);
	setupResizeRotateHoverPoint(rrHover);

	rrHover = new ResizeRotateCorner(this, ResizeRotateHoverPoint::Right | ResizeRotateHoverPoint::Top);
	setupResizeRotateHoverPoint(rrHover);

	rrHover = new ResizeRotateCorner(this, ResizeRotateHoverPoint::Right | ResizeRotateHoverPoint::Bottom);
	setupResizeRotateHoverPoint(rrHover);
}

void TransformableElement::updateHoverPoints()
{
	foreach (HoverPoint* point, hoverPointList)
		point->updateAnchor();
}

void TransformableElement::destroyHoverPoints()
{
	foreach (HoverPoint* point, hoverPointList)
		delete point;

	hoverPointList.clear();
}

void TransformableElement::addHoverPointToList(HoverPoint *point)
{
	hoverPointList << point;
}

void TransformableElement::removeHoverPointFromList(HoverPoint *point)
{
	hoverPointList.removeAll(point);
}

void TransformableElement::setupResizeRotateHoverPoint(ResizeRotateHoverPoint *point)
{
	connect(point, SIGNAL(resizeRotate(qreal, qreal)), this, SLOT(onResizeRotate(qreal, qreal)));
	connect(point, SIGNAL(pressed()), this, SLOT(onResizeRotateHoverPress()));
	connect(point, SIGNAL(released()), this, SLOT(onResizeRotateHoverRelease()));

	addHoverPointToList(point);
}

void TransformableElement::onMenu(const QPointF &pos)
{
	QGraphicsView *view = scene()->views().first();
	
	QPointF menuPos = view->mapToGlobal(view->mapFromScene(mapToScene(pos)));
	
	hoverContextMenu = true;

	showContextMenu(menuPos.toPoint());
}

void TransformableElement::onResizeRotateHoverRelease()
{ 
	hoverUnderMousePress = false;
	
	finishChanging();
}

void TransformableElement::onResizeRotate(qreal diffScale, qreal diffAngle)
{
	startChanging();

	if (isSilentChanging())
		return;

	emit resizeRotate(this, diffScale, diffAngle);	
}

void TransformableElement::onContextMenuShow()
{
	contextMenuActive = true;
}

void TransformableElement::onContextMenuHide()
{
	contextMenuActive = false;
	hoverContextMenu = false;

	// Mouse could move off of the current item
	if (!isUnderMouse())
		emit mouseLeave(this);
}

}