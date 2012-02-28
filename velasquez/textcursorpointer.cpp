#include "textcursorpointer.h"

#include <QGraphicsScene>
#include <QPainter>
#include <QApplication>
#include <QPolygonF>

#include "textelement.h"
#include "debug.h"

namespace Velasquez
{

// Check for placement of point at the horizontal/vertical scene bound
bool pointInBound(const QPointF &point, const QLineF &bound, bool strict = false)
{
	if (bound.y1() == bound.y2())
	{
		if ( point.x() >= bound.x1() && point.x() <= bound.x2() )
			return (strict) ? bound.y1() == point.y() : true;
	}
	else
		if ( point.y() >= bound.y1() && point.y() <= bound.y2() )
			return (strict) ? bound.x1() == point.x() : true;

	return false;
}

TextCursorPointer::TextCursorPointer(QGraphicsItem *parentItem)
	: QObject(NULL), QGraphicsItem(parentItem), active(false), timerId(-1)
{
	// Fill with empty lines
	for (int i = 0; i < 4; i ++)
		sceneBounds.append(QLineF());

	setVisible(false);
	setZValue(CURSOR_POINTER_ZVALUE);
}

void TextCursorPointer::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
	Q_UNUSED(option)
	Q_UNUSED(widget)
	
	if (isActive())
	{
		painter->setRenderHint(QPainter::Antialiasing);

		QPen pen(Qt::red);
		pen.setWidth(2);
		painter->setPen(pen);

		QRectF drawingRect = boundingRect().adjusted(pen.width(), pen.width(), -pen.width(), -pen.width());
		drawingRect.adjust(pen.width() * 2, pen.width() * 2, -pen.width() * 2, -pen.width() * 2);

		const QPointF arrowPoints[6] =
		{			
			QPointF(drawingRect.left(), drawingRect.center().y()),
			QPointF(drawingRect.right(), drawingRect.center().y()),

			QPointF(drawingRect.right() - drawingRect.width() / 3, drawingRect.top()),
			QPointF(drawingRect.right(), drawingRect.center().y()),
			
			QPointF(drawingRect.right() - drawingRect.width() / 3, drawingRect.bottom()),
			QPointF(drawingRect.right(), drawingRect.center().y()),			
		};

		painter->drawLines(arrowPoints, 3);
	}
}

QRectF TextCursorPointer::boundingRect() const
{
	static QRectF rect(-CURSOR_POINTER_WIDTH / 2, -CURSOR_POINTER_HEIGHT / 2, CURSOR_POINTER_WIDTH, CURSOR_POINTER_HEIGHT);

	return rect;
}

void TextCursorPointer::setActive(bool active)
{
	if (this->active == active)
		return;

	if (!active)
		stopBlinking();

	this->active = active;
}

void TextCursorPointer::pointToCursor(TextElement *element)
{
	if (!isActive())
		return;

	QPolygonF cursorPoly = element->mapToScene(element->getCursorRect());
	QPolygonF scenePoly(sceneRect);

	if (!scenePoly.intersected(cursorPoly).isEmpty())
	{
		stopBlinking();
		return;
	}

	QPointF newPos;
	qreal angle;

	getUpdate(cursorPoly, newPos, angle);

	setPos(newPos);
	resetTransform();
	rotate(angle);

	startBlinking();
}

QVariant TextCursorPointer::itemChange(GraphicsItemChange change, const QVariant &value)
{
	if (change == ItemSceneChange)
	{
		if (scene() != NULL)
			disconnect(scene(), SIGNAL(sceneRectChanged(const QRectF &)), this, SLOT(onSceneRectChanged(const QRectF &)));

		QGraphicsScene *newScene = value.value <QGraphicsScene *>();

		if (newScene != NULL)
		{
			connect(newScene, SIGNAL(sceneRectChanged(const QRectF &)), this, SLOT(onSceneRectChanged(const QRectF &)));
			
			onSceneRectChanged(newScene->sceneRect());
		}
	}

	return QGraphicsItem::itemChange(change, value);
}

void TextCursorPointer::startBlinking()
{
	if (timerId > 0)
		return;

	timerId = startTimer(QApplication::cursorFlashTime() / 2);
	
	setVisible(true);
}

void TextCursorPointer::stopBlinking()
{
	if (timerId < 0)
		return;

	killTimer(timerId);
	timerId = -1;
	
	setVisible(false);
}

void TextCursorPointer::timerEvent(QTimerEvent *event)
{
	Q_UNUSED(event)
	
	setVisible(!isVisible());
}

int TextCursorPointer::getSceneBoundIntersection(const QLineF &cursorMidLine, QPointF &newPos, qreal &newAngle) const
{
	int closestBoundIndex = -1;
	qreal shortestLength = -1;	
	
	// Try to find visible intersection of cursor rect midline[-] with scene bound
	for (int i = 0; i <= 3; i++)
	{
		QPointF testPoint;
		QLineF bound = sceneBounds[i];

		if (cursorMidLine.intersect(bound, &testPoint))
		{
			// Visibility check
			if (!pointInBound(testPoint, bound))
				continue;

			// Select closest bound
			qreal len = QLineF(cursorMidLine.p1(), testPoint).length();
			
			if (shortestLength < 0 || shortestLength > len)
			{
				shortestLength = len;
				closestBoundIndex = i;
				
				newPos = testPoint;				
			}
		}
	}
	
	// Get angle if found bound
	if (closestBoundIndex >= 0)
	{
		QLineF sceneBeam(cursorMidLine.p1(), newPos);		
		
		QLineF closestBound = sceneBounds[closestBoundIndex];
		newAngle = sceneBeam.angleTo(closestBound) - 90;
		
		if (closestBound.y1() == closestBound.y2())
			newAngle -= 90;
	}

	return closestBoundIndex;
}

int TextCursorPointer::getNotIntersectedUpdate(const QLineF &cursorMidLine, QPointF &newPos, qreal &newAngle, int &crossBoundIndex) const
{
	int closestBoundIndex = 0;
	crossBoundIndex = -1;

	// Check for "crossed" locations
	if (cursorMidLine.p1().x() > sceneRect.topRight().x() && cursorMidLine.p1().y() < sceneRect.topRight().y())
	{
		closestBoundIndex = 0;
		crossBoundIndex = 1;

		newPos = sceneRect.topRight();
	}
	else
		if (cursorMidLine.p1().x() < sceneRect.topLeft().x() && cursorMidLine.p1().y() < sceneRect.topLeft().y())
		{
			closestBoundIndex = 1;
			crossBoundIndex = 2;

			newPos = sceneRect.topLeft();
		}
		else
			if (cursorMidLine.p1().x() < sceneRect.bottomLeft().x() && cursorMidLine.p1().y() > sceneRect.bottomLeft().y())
			{
				closestBoundIndex = 2;
				crossBoundIndex = 3;

				newPos = sceneRect.bottomLeft();				
			}
			else
				if (cursorMidLine.p1().x() > sceneRect.bottomRight().x() && cursorMidLine.p1().y() > sceneRect.bottomRight().y())
				{
					closestBoundIndex = 3;
					crossBoundIndex = 0;

					newPos = sceneRect.bottomRight();
				}

	if (crossBoundIndex < 0)
	{	
		// Fit into the scene
		newPos.setX(qMax(sceneRect.left(), cursorMidLine.p1().x()));
		newPos.setY(qMax(sceneRect.top(), cursorMidLine.p1().y()));

		newPos.setX(qMin(sceneRect.right(), newPos.x()));
		newPos.setY(qMin(sceneRect.bottom(), newPos.y()));

		// Find angle
		for (int i = 0; i <= 3; i++)
		{
			if (pointInBound(newPos, sceneBounds[i], true))
			{
				newAngle = i * -90;
				closestBoundIndex = i;

				break;
			}
		}
	}
	else
		newAngle = closestBoundIndex * -90 - 45;

	return closestBoundIndex;
}

void TextCursorPointer::getUpdate(const QPolygonF &cursorPolygon, QPointF &newPos, qreal &newAngle) const
{
	// Depending on scene bound we need to adjust position of pointer
	static QPointF posAjustments[4] = { QPointF(-1, 0), QPointF(0, 1), QPointF(1, 0), QPointF(0, -1) };

	// Prepare the midline
	QPointF p1 = cursorPolygon[1] + (cursorPolygon[2] - cursorPolygon[1]) / 2;
	QPointF p2 = cursorPolygon[0] + (cursorPolygon[3] - cursorPolygon[0]) / 2;

	QLineF cursorMidLine = QLineF(p1, p2);	

	// Index of closest bound and crossed one, if any
	int closestBoundIndex, crossBoundIndex = -1;
	
	// Try to find visible intersection of cursor rect midline[-] with scene bound
	closestBoundIndex = getSceneBoundIntersection(cursorMidLine, newPos, newAngle);

	// Found intersection with scene?
	if (closestBoundIndex < 0)
		closestBoundIndex = getNotIntersectedUpdate(cursorMidLine, newPos, newAngle, crossBoundIndex);

	// Now adjust pos
	QPointF adjustment = posAjustments[closestBoundIndex];

	if (crossBoundIndex >= 0)
		adjustment += posAjustments[crossBoundIndex];

	newPos.rx() += adjustment.x() * qMax(CURSOR_POINTER_WIDTH / 2, CURSOR_POINTER_HEIGHT / 2);
	newPos.ry() += adjustment.y() * qMax(CURSOR_POINTER_WIDTH / 2, CURSOR_POINTER_HEIGHT / 2);
}

void TextCursorPointer::onSceneRectChanged(const QRectF &rect)
{
	// All scene bounds in counter-clockwise order from smaller to higher coordinates
	sceneRect = rect;

	// All scene bounds in counter-clockwise order from smaller to higher coordinates
	sceneBounds[0].setPoints(sceneRect.topRight(), sceneRect.bottomRight());
	sceneBounds[1].setPoints(sceneRect.topLeft(), sceneRect.topRight());
	sceneBounds[2].setPoints(sceneRect.topLeft(), sceneRect.bottomLeft());
	sceneBounds[3].setPoints(sceneRect.bottomLeft(), sceneRect.bottomRight());

	update();
}

}
