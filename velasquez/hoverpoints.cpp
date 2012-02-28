#include "hoverpoints.h"

#include <QGraphicsSceneMouseEvent>
#include <QPainter>
#include <QCursor>
#include <QtGlobal>
#include <math.h>
#include <QApplication>
#include <QLinearGradient>

#include "transformableelement.h"
#include "drawingutils.h"
#include "vcommon.h"
#include "debug.h"

namespace Velasquez
{

// HoverPoint class
HoverPoint::HoverPoint(TransformableElement *parentElement)
	: QObject(), QGraphicsItem(parentElement), underMousePress(false)
{
	this->parentElement = parentElement;

	setFlag(QGraphicsItem::ItemIsFocusable, false);
	setFlag(QGraphicsItem::ItemIsSelectable, false);

	setZValue(HOVER_POINTS_ZVALUE);
}

void HoverPoint::updateAnchor()
{
	setPos(getAnchor());

	resetTransform();

	// The size of hover point should not change
	qreal unScale = 1 / parentElement->getTransformation().getScale();
	scale(unScale, unScale);

	update();
}

void HoverPoint::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
    if (!isActive())
	{
		event->ignore();
		return;
	}

	event->accept();

	underMousePress = true;

	emit pressed(this->pos());
}

void HoverPoint::mouseReleaseEvent(QGraphicsSceneMouseEvent * event)
{
    if (!underMousePress)
		return;

	event->accept();

    underMousePress = false;
	
	emit released(this->pos());
}

bool HoverPoint::isActive() const
{
	return parentElement->isSelected();
}

QPolygonF HoverPoint::getParentBounds() const
{
	QPolygonF bounds = parentElement->boundingRect();

	return bounds;
}

// RemoveHoverPoint class
RemoveHoverPoint::RemoveHoverPoint(TransformableElement *parentElement)
	: HoverPoint(parentElement)
{
	setZValue(HOVER_POINTS_ZVALUE - 1);

	setToolTip(tr(REMOVE_HOVER_POINT_TOOLTIP));
}

QRectF RemoveHoverPoint::boundingRect() const
{
	static QRectF rect(-REMOVE_HOVER_POINT_SIZE / 2, -REMOVE_HOVER_POINT_SIZE / 2, REMOVE_HOVER_POINT_SIZE, REMOVE_HOVER_POINT_SIZE);

	return rect;
}

void RemoveHoverPoint::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
	Q_UNUSED(option)
	Q_UNUSED(widget)
	
	if (isActive())
	{
		painter->setRenderHint(QPainter::Antialiasing);

		QPen pen(Qt::black);
		pen.setWidth(1);

		painter->setPen(pen);
		painter->setBrush(QColor(Qt::white));
		
		// drawing bounding rectangle
		QRectF drawingRect = boundingRect().adjusted(pen.width(), pen.width(), -pen.width(), -pen.width());
		painter->drawRect(drawingRect);

		// drawing red cross
		pen.setWidth(1);
		pen.setColor(Qt::darkRed);
		painter->setPen(pen);
		drawingRect.adjust(pen.width() * 2, pen.width() * 2, -pen.width() * 2, -pen.width() * 2);

		const QPointF arrowPoints[4] =
		{
			drawingRect.bottomLeft(),
			drawingRect.topRight(),
			drawingRect.topLeft(),
			drawingRect.bottomRight()
		};

		painter->drawLines(arrowPoints, 2);
	}
}

QPointF RemoveHoverPoint::getAnchor() const
{
	QPointF anchorPoint;

	QPolygonF bounds = getParentBounds();

	anchorPoint.setX( bounds[0].x() + (bounds[1].x() - bounds[0].x()) / 2 );
	anchorPoint.setY( bounds[0].y() + (bounds[1].y() - bounds[0].y()) / 2 );

	return anchorPoint;
}

// ResizeRotateHoverPoint class
ResizeRotateHoverPoint::ResizeRotateHoverPoint(TransformableElement *parentElement, Location location)
	: HoverPoint(parentElement), inStickyMode(true)
{
	this->location = location;

	setCursor(Qt::PointingHandCursor);
}

QRectF ResizeRotateHoverPoint::boundingRect() const
{
	static QRectF rect(-RESIZE_ROTATE_HOVER_POINT_SIZE / 2, -RESIZE_ROTATE_HOVER_POINT_SIZE / 2, RESIZE_ROTATE_HOVER_POINT_SIZE, RESIZE_ROTATE_HOVER_POINT_SIZE);

	return rect;
}

void ResizeRotateHoverPoint::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
	Q_UNUSED(option)
	Q_UNUSED(widget)
	
	if (isActive())
	{
		painter->setRenderHint(QPainter::Antialiasing);

		QPen pen(Qt::black);
		pen.setWidth(1);

		painter->setPen(pen);
		painter->setBrush(QColor(Qt::white));
		
		// drawing bounding rectangle
		QRectF drawingRect = boundingRect().adjusted(pen.width(), pen.width(), -pen.width(), -pen.width());
		painter->drawRect(drawingRect);

		// drawing arrow
		pen.setWidth(1);
		painter->setPen(pen);
		drawingRect.adjust(pen.width() * 2, pen.width() * 2, -pen.width() * 2, -pen.width() * 2);

		const QPointF arrowPoints[10] =
		{
			QPointF(drawingRect.bottomLeft().x(), drawingRect.bottomLeft().y() - drawingRect.height() / 3),
			drawingRect.bottomLeft(),

			drawingRect.bottomLeft(),
			QPointF(drawingRect.bottomLeft().x() + drawingRect.width() / 3, drawingRect.bottomLeft().y()),

			QPointF(drawingRect.topRight().x() - drawingRect.height() / 3, drawingRect.topRight().y()),
			drawingRect.topRight(),

			drawingRect.topRight(),
			QPointF(drawingRect.topRight().x(), drawingRect.topRight().y() + drawingRect.height() / 3),

			drawingRect.bottomLeft(),
			drawingRect.topRight()
		};

		painter->drawLines(arrowPoints, 5);
	}
}

void ResizeRotateHoverPoint::updateAnchor()
{
	HoverPoint::updateAnchor();

	// if we paint in left bottom or right top we need to rotate on 90 degs
	if ( (this->location & Left && this->location & Top) || (this->location & Right && this->location & Bottom) )
		rotate(90);

	// if parent is reflected we need to rotate on 90 degs too
	if ( parentElement->getTransformation().isReflected() )
		rotate(90);
}

void ResizeRotateHoverPoint::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
	HoverPoint::mousePressEvent(event);

    startPos = event->scenePos();
	anchorCorrection = mapToParent(event->pos()) - getAnchor();
}

void ResizeRotateHoverPoint::mouseMoveEvent(QGraphicsSceneMouseEvent *event)
{
	HoverPoint::mouseMoveEvent(event);

	if (!isUnderMousePress())
        return;

    event->accept();

    changeScaleAndAngle(event->scenePos());
}

void ResizeRotateHoverPoint::mouseReleaseEvent(QGraphicsSceneMouseEvent * event)
{
    HoverPoint::mouseReleaseEvent(event);

    startPos = QPointF();
}

QPointF ResizeRotateHoverPoint::getAnchor() const
{
	QPointF anchorPoint;

	QPolygonF bounds = getParentBounds();
	
	Location correctedLocation = getLocationAfterTransform();

	if ( (correctedLocation & Left) && (location & Top) )
		anchorPoint = bounds[0];
	else
		if ( (correctedLocation & Right) && (location & Top) )
			anchorPoint = bounds[1];
		else 
			if ( (correctedLocation & Right) && (location & Bottom) )
				anchorPoint = bounds[2];
			else
				if ( (correctedLocation & Left) && (location & Bottom) )
					anchorPoint = bounds[3];

	return anchorPoint;
}

ResizeRotateHoverPoint::Location ResizeRotateHoverPoint::getLocationAfterTransform() const
{
	Transformation transformation = parentElement->getTransformation();
	Location correctedLocation = location;

	if (transformation.isReflected())
	{
		if (location & Left)
		{
			correctedLocation ^= Left;
			correctedLocation |= Right;
		}

		if (location & Right)
		{
			correctedLocation ^= Right;
			correctedLocation |= Left;
		}
	}
	
	return correctedLocation;
}

void ResizeRotateHoverPoint::changeScaleAndAngle(const QPointF &newPos)
{
	if (newPos == startPos)
		return;

	inStickyMode = !(QApplication::keyboardModifiers() & Qt::AltModifier);

	qreal xOldLen, yOldLen;
	qreal xNewLen, yNewLen;

	QPointF center = parentElement->mapToScene(parentElement->boundingRect().center());

	// We need to know measure from the anchor + initial press pos correction
	QPointF changeAnchor = parentElement->mapToScene(getAnchor() + anchorCorrection);

	xOldLen = changeAnchor.x() - center.x();
	yOldLen = changeAnchor.y() - center.y();

	xNewLen = newPos.x() - center.x();
	yNewLen = newPos.y() - center.y();

	qreal oldLen = DrawingUtils::segmentLength(changeAnchor, center);
	qreal newLen = DrawingUtils::segmentLength(newPos, center);

	if (oldLen == 0)
		return;

	// get safe proportion
	qreal diffScale = newLen / oldLen;

	if (!isSafeScale(diffScale))
		return;
	
	// and angle difference
	qreal srcAngle = atan2(yOldLen, xOldLen);
	qreal newAngle = atan2(yNewLen, xNewLen);

	qreal diffAngle = getStickyRotationAngle(180.0 * (newAngle - srcAngle) / PI, newLen);

	// have we really got something
	if (diffAngle == 0 && diffScale == 1)
		return;
	emit resizeRotate(diffScale, diffAngle);

	startPos = newPos;
}

qreal ResizeRotateHoverPoint::getStickyRotationAngle(qreal diffAngle, qreal radius) const
{
	if (inStickyMode)
	{
		qreal currentAngle = parentElement->getTransformation().getAngle();
		qreal newAngle = currentAngle + diffAngle;

		// get nearest sticked angle
		qint32 n = qRound(newAngle / STICKY_ROTATION_ANGLE);
		qreal stickedAngle = n * STICKY_ROTATION_ANGLE;

		// get delta angle to calc the arc len
		qreal deltaAngle = newAngle - stickedAngle;

		// get arc length
		qreal deltaLength = deltaAngle * radius * PI / 180;

		// if we are inside sticky zone we need to accumulate rotation and jump to sticky angle
		if (qAbs(deltaLength) < STICKY_ROTATION_DISTANCE)
		{
			// new angle must be equal to sticked
			diffAngle = stickedAngle - currentAngle;
		}
	}

	return diffAngle;
}

bool ResizeRotateHoverPoint::isSafeScale(qreal diffScale) const
{
	return (parentElement->isValidTransformation(Transformation(diffScale), true));
}

// ReflectHoverPoint class
ReflectHoverPoint::ReflectHoverPoint(TransformableElement *parentElement)
	: HoverPoint(parentElement)
{
	setZValue(HOVER_POINTS_ZVALUE - 1);

	setToolTip(tr(REFLECT_HOVER_POINT_TOOLTIP));
}

QRectF ReflectHoverPoint::boundingRect() const
{
	static QRectF rect(-REFLECT_HOVER_POINT_SIZE / 2, -REFLECT_HOVER_POINT_SIZE / 2, REFLECT_HOVER_POINT_SIZE, REFLECT_HOVER_POINT_SIZE);

	return rect;
}

void ReflectHoverPoint::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
	Q_UNUSED(option)
	Q_UNUSED(widget)

	if (isActive())
	{
		painter->setRenderHint(QPainter::Antialiasing);

		QPen pen(Qt::black);
		pen.setWidth(1);

		painter->setPen(pen);
		painter->setBrush(QColor(Qt::white));
		
		// drawing bounding rectangle
		QRectF drawingRect = boundingRect().adjusted(pen.width(), pen.width(), -pen.width(), -pen.width());
		painter->drawRect(drawingRect);

		const QPointF arrowPoints[8] =
		{
			QPointF(drawingRect.left() + drawingRect.width() / 2.5, drawingRect.top() + drawingRect.height() / 5),
			QPointF(drawingRect.left() + drawingRect.width() / 5, drawingRect.top() + drawingRect.height() / 2.5),
			
			QPointF(drawingRect.left() + drawingRect.width() / 5, drawingRect.top() + drawingRect.height() / 2.5),
			QPointF(drawingRect.right() - drawingRect.width() / 5, drawingRect.top() + drawingRect.height() / 2.5),
			
			QPointF(drawingRect.left() + drawingRect.width() / 5, drawingRect.bottom() - drawingRect.height() / 2.5),
			QPointF(drawingRect.right() - drawingRect.width() / 5, drawingRect.bottom() - drawingRect.height() / 2.5),

			QPointF(drawingRect.right() - drawingRect.width() / 5, drawingRect.bottom() - drawingRect.height() / 2.5),
			QPointF(drawingRect.right() - drawingRect.width() / 2.5,  drawingRect.bottom() - drawingRect.height() / 5)
		};

		painter->drawLines(arrowPoints, 4);
	}
}

QPointF ReflectHoverPoint::getAnchor() const
{
	QPointF anchorPoint;

	QPolygonF bounds = getParentBounds();

	anchorPoint.setX( bounds[3].x() + (bounds[2].x() - bounds[3].x()) / 2);
	anchorPoint.setY( bounds[3].y() + (bounds[2].y() - bounds[3].y()) / 2);

	return anchorPoint;
}

// ResizeRotateCorner class
ResizeRotateCorner::ResizeRotateCorner(TransformableElement *parentElement, Location location)
	: ResizeRotateHoverPoint(parentElement, location), side(0)
{
	updateSide();
}

QRectF ResizeRotateCorner::boundingRect() const
{
	return QRectF(0, 0, side, side);
}

void ResizeRotateCorner::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
	Q_UNUSED(option)
	Q_UNUSED(widget)

	if (isActive())
	{
		painter->setRenderHint(QPainter::Antialiasing);

		// draw outer border
		QColor borderColor = QColor(ELEMENT_SELECTION_BORDER_COLOR1);

		QPen pen(borderColor);
		QBrush brush(borderColor);

		pen.setWidthF(1.0 / parentElement->getTransformation().getScale());
		pen.setStyle(Qt::DotLine);
		painter->setPen(pen);
		
		qreal penWidth = pen.widthF() * 1.5;
		QRectF outerRect = boundingRect().adjusted(penWidth, penWidth, -penWidth, -penWidth);
		painter->drawPath(getShapePath(outerRect, Drawing));

		// draw inner border
		borderColor = QColor(ELEMENT_SELECTION_BORDER_COLOR2);

		borderColor.setAlphaF(0.4);
		pen.setColor(borderColor);

		borderColor.setAlphaF(0.2);
		brush.setColor(borderColor);

		painter->setPen(pen);
		
		penWidth = pen.widthF() * 2.5;
		QRectF innerRect = boundingRect().adjusted(penWidth, penWidth, -penWidth, -penWidth);

		painter->drawPath(getShapePath(innerRect, Drawing));

		// draw interior
		painter->setPen(Qt::NoPen);
		painter->setBrush(brush);

		penWidth = pen.widthF() / 2.0;
		QRectF fillRect = boundingRect().adjusted(penWidth, penWidth, -penWidth, -penWidth);

		painter->drawPath(getShapePath(fillRect, Bounds));
	}
}

QPainterPath ResizeRotateCorner::shape() const
{
	return getShapePath(boundingRect(), Bounds);
}

void ResizeRotateCorner::updateAnchor()
{
	if (updateSide())
		update();

	setPos(getAnchor());	
}

QPointF ResizeRotateCorner::getAnchor() const
{
	QPointF anchor = ResizeRotateHoverPoint::getAnchor();

	if (getLocation() & Right)
		anchor.rx() -= side;

	if (getLocation() & Bottom)
		anchor.ry() -= side;

	return anchor;
}

ResizeRotateCorner::Location ResizeRotateCorner::getLocationAfterTransform() const
{
	// Use original location
	return getLocation();
}

bool ResizeRotateCorner::updateSide()
{
	qreal newSide = qMin(parentElement->boundingRect().width(), parentElement->boundingRect().height()) * RESIZE_ROTATE_CORNER_SIZE_PERCENTS / 100.0;
	
	if (side == newSide)
		return false;
	
	prepareGeometryChange();
	side = newSide;
	
	return true;
}

QPainterPath ResizeRotateCorner::getShapePath(const QRectF &rect, GetShapePathMode mode) const
{
	QPainterPath path;
	qreal startAngle = 0;
	qreal leftShift = -rect.width(), topShift = -rect.height();
	
	QList <QPointF> bounds;

	if ( (getLocation() & Left) && (getLocation() & Top) )
	{
		bounds << rect.topRight() << rect.topLeft() << rect.bottomLeft();
		startAngle = 270;
	}

	if ( (getLocation() & Right) && (getLocation() & Top) )
	{
		bounds << rect.bottomRight() << rect.topRight() << rect.topLeft();
		leftShift = 0;
		startAngle = 180;
	}

	if ( (getLocation() & Right) && (getLocation() & Bottom) )
	{
		bounds << rect.bottomLeft() << rect.bottomRight() << rect.topRight();
		startAngle = 90;
		leftShift = 0;
		topShift = 0;
	}

	if ( (getLocation() & Left) && (getLocation() & Bottom) )
	{
		bounds << rect.topLeft() << rect.bottomLeft() << rect.bottomRight();
		topShift = 0;
	}

	// For checking of bounds we need a complete sector path
	if (mode == Bounds)
	{
		path.moveTo(bounds[0]);
		path.lineTo(bounds[1]);
		path.lineTo(bounds[2]);
	}
	else
	{
		path.moveTo(bounds[2]);
	}

	QRectF arcRect(rect.left() + leftShift, rect.top() + topShift, rect.width() * 2, rect.height() * 2);
	path.arcTo(arcRect, startAngle, 90);

	return path;
}

// MenuHoverPoint class
MenuHoverPoint::MenuHoverPoint(TransformableElement *parentElement)
	: HoverPoint(parentElement)
{
	setZValue(HOVER_POINTS_ZVALUE + 1);

	setToolTip(tr(MENU_HOVER_POINT_TOOLTIP));

	setCursor(Qt::PointingHandCursor);
	setAcceptHoverEvents(true);
}

QRectF MenuHoverPoint::boundingRect() const
{
	static QRectF rect(-MENU_HOVER_POINT_SIZE / 2.0, -MENU_HOVER_POINT_SIZE / 2.0, MENU_HOVER_POINT_SIZE, MENU_HOVER_POINT_SIZE);

	return rect;
}

void MenuHoverPoint::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
	Q_UNUSED(option)
	Q_UNUSED(widget)
	
	if (isActive())
	{
		painter->setRenderHint(QPainter::Antialiasing);

		QPen pen(0x5EC290);
		pen.setWidth(1);

		painter->setPen(pen);
		painter->setBrush(QColor(ELEMENT_SELECTION_BORDER_COLOR2));
		
		// drawing bounding ellipse
		QRectF drawingRect = boundingRect().adjusted(0, 0, -pen.width(), -pen.width());
		painter->drawEllipse(drawingRect);

		const qreal sizeScale = 0.32;

		// drawing triangle
		const QPointF trianglePoints[3] =
		{
			QPointF(drawingRect.center().x() - drawingRect.width() * sizeScale, drawingRect.center().y() - drawingRect.height() * sizeScale + 1),
			QPointF(drawingRect.center().x() + drawingRect.width() * sizeScale, drawingRect.center().y() - drawingRect.height() * sizeScale + 1),
			QPointF(drawingRect.center().x(), drawingRect.center().y() + drawingRect.height() * sizeScale + pen.widthF()),
		};

		QPointF top = trianglePoints[0];
		top.setX(drawingRect.center().x());

		QLinearGradient gradient(top, trianglePoints[2]);
		gradient.setColorAt(0, QColor(0x399165));
		gradient.setColorAt(1, QColor(0x10261A));

		painter->setBrush(gradient);
		painter->drawPolygon(trianglePoints, 3);
	}
}

QPointF MenuHoverPoint::getAnchor() const
{
	QPointF anchorPoint;

	QPolygonF bounds = getParentBounds();

	anchorPoint.setX( bounds[3].x() + (bounds[2].x() - bounds[3].x()) / 2);
	anchorPoint.setY( bounds[3].y() + (bounds[2].y() - bounds[3].y()) / 2);

	return anchorPoint;
}

}
