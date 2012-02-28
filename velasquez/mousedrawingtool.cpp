#include "mousedrawingtool.h"

#include <QGraphicsSceneMouseEvent>

#include "mousedrawingelement.h"

namespace Velasquez
{

MouseDrawingTool::MouseDrawingTool(QObject *parent)
	: DrawingTool(parent), currentElement(NULL)
{
}

bool MouseDrawingTool::hasCursor() const
{
	return true;
}

QCursor MouseDrawingTool::getDefaultCursor() const
{
	return QCursor(Qt::CrossCursor);
}

QStringList MouseDrawingTool::getFileExtensions() const
{
	return QStringList();
}

bool MouseDrawingTool::canCreate(QGraphicsSceneMouseEvent *mouseEvent) const
{
	Q_UNUSED(mouseEvent)

	return true;
}

bool MouseDrawingTool::canCreate(QKeyEvent *keyEvent) const 
{
	Q_UNUSED(keyEvent)
	
	return false; 
}

bool MouseDrawingTool::canCreate(const QMimeData *data) const 
{ 
	Q_UNUSED(data)
	
	return false; 
}

void MouseDrawingTool::createElement(QGraphicsSceneMouseEvent *mouseEvent)
{
	bool pressed = mouseEvent->buttons() & Qt::LeftButton;

	if (!pressed)
	{
		if (currentElement != NULL)
		{			
			finishCreating();
			currentElement = NULL;
		}

		return;
	}
	else
	{
		if (currentElement == NULL)
		{
			currentElement = createNewElement();

			setupNewElement(currentElement, mouseEvent->scenePos());
			startCreating();
		}

		runCreating(mouseEvent);
	}
}

void MouseDrawingTool::createElement(QKeyEvent *keyEvent) 
{
	Q_UNUSED(keyEvent)
}

void MouseDrawingTool::createElement(const QMimeData *data, const QPointF &pos)
{
	Q_UNUSED(data)
	Q_UNUSED(pos)
}

void MouseDrawingTool::startCreating()
{
	currentElement->startDrawing();
}

void MouseDrawingTool::runCreating(QGraphicsSceneMouseEvent *mouseEvent)
{
	currentElement->addPoint(currentElement->mapFromScene(mouseEvent->scenePos()));
}

void MouseDrawingTool::finishCreating()
{
	currentElement->finishDrawing();
}

}