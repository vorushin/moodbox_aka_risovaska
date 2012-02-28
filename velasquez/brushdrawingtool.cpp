#include "brushdrawingtool.h"

#include <QTimerEvent>
#include <QCursor>
#include <QGraphicsView>

#include "brushdrawingelement.h"
#include "editorscene.h"

namespace Velasquez
{

BrushDrawingTool::BrushDrawingTool(QObject *parent)
	: MouseDrawingTool(parent), pollingTimerId(-1)
{
}

void BrushDrawingTool::startCreating()
{
	BrushDrawingElement *brushElement = qgraphicsitem_cast<BrushDrawingElement *>(currentElement);
	
	if (brushElement != NULL)
	{
		int i = brushElement->getPollingInterval();

		if (i > 0)
			startPollingTimer(i);
	}

	MouseDrawingTool::startCreating();
}

void BrushDrawingTool::runCreating(QGraphicsSceneMouseEvent *mouseEvent)
{
	if (!isPollingRunning())
		MouseDrawingTool::runCreating(mouseEvent);
}

void BrushDrawingTool::finishCreating()
{
	MouseDrawingTool::finishCreating();

	if (isPollingRunning())
		stopPollingTimer();
}

void BrushDrawingTool::timerEvent(QTimerEvent *event)
{
	if (event->timerId() != pollingTimerId)
		return;

	pollAndAddMouse();
}

void BrushDrawingTool::pollAndAddMouse()
{
	if (currentElement == NULL)
		return;

	QGraphicsView *sceneView = scene->views().first();	

	QPoint viewPos = sceneView->mapFromGlobal(QCursor::pos());
	QPointF scenePos = sceneView->mapToScene(viewPos);
	
	currentElement->addPoint(currentElement->mapFromScene(scenePos));
}

void BrushDrawingTool::startPollingTimer(int interval)
{
	pollingTimerId = startTimer(interval);
}

void BrushDrawingTool::stopPollingTimer()
{
	killTimer(pollingTimerId);
	pollingTimerId = -1;
}

}