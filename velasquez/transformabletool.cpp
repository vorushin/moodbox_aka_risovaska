#include "transformabletool.h"

#include <QUndoStack>

#include "transformableelement.h"
#include "editorscene.h"
#include "undocommands.h"

namespace Velasquez
{

TransformableTool::TransformableTool(QObject *parent)
	: DrawingTool(parent), sessionNumber(0), sessionStarted(false)
{
}

bool TransformableTool::addElement(DrawingElement *element)
{
	if (!DrawingTool::addElement(element))
		return false;
	
	TransformableElement *transformableElement = qgraphicsitem_cast<TransformableElement *> (element);

	scene->addDecorationTypes(transformableElement->getDecorationTypes(), EditorScene::ItemDecoration);

	connectElement(transformableElement);
	
	return true;
}

void TransformableTool::setupNewElement(TransformableElement *element, const QPointF &pos)
{
	DrawingTool::setupNewElement(element, pos);

	resizeNewElement(element);
	scene->addDecorationTypes(element->getDecorationTypes(), EditorScene::ItemDecoration);

	connectElement(element);
}

void TransformableTool::extractElement(DrawingElement *element)
{
	DrawingTool::extractElement(element);

	disconnectElement(qgraphicsitem_cast<TransformableElement *> (element));
}

void TransformableTool::resizeNewElement(TransformableElement *element)
{
	// Already scaled? Do not scale again
	if (element->getTransformation().isScaled())
		return;

	QSizeF newSize = scene->sceneRect().size();
	QSizeF currentSize = element->boundingRect().size();

	// Do not need to resize small images
	if (currentSize.width() <= newSize.width() && currentSize.height() <= newSize.height())
		return;

	newSize.setWidth(newSize.width() / 2);
	newSize.setHeight(newSize.height() / 2);
	
	element->transformToFit(newSize);
}

void TransformableTool::connectElement(TransformableElement *element)
{
	connect(element, SIGNAL(changing(bool)), this, SLOT(onChangingSession(bool)));
	
	connect(element, SIGNAL(mouseEnter(TransformableElement *)), this, SLOT(onElementMouseEnter(TransformableElement *)));
	connect(element, SIGNAL(mouseMove(TransformableElement *)), this, SLOT(onElementMouseMove(TransformableElement *)));
	connect(element, SIGNAL(mouseLeave(TransformableElement *)), this, SLOT(onElementMouseLeave(TransformableElement *)));
	
	connect(element, SIGNAL(moved(TransformableElement *, const QPointF &)), this, SLOT(onElementMoved(TransformableElement *, const QPointF &)));
	connect(element, SIGNAL(remove(DrawingElement *)), this, SLOT(onRemoveElement(DrawingElement *)));
	connect(element, SIGNAL(resizeRotate(TransformableElement *, qreal, qreal)), this, SLOT(onResizeRotateElement(TransformableElement *, qreal, qreal)));
	connect(element, SIGNAL(reflect(TransformableElement *)), this, SLOT(onReflectElement(TransformableElement *)));

	connect(element, SIGNAL(bringFront(TransformableElement *)), this, SLOT(onBringFrontElement(TransformableElement *)));
	connect(element, SIGNAL(moveBackward(TransformableElement *)), this, SLOT(onMoveBackwardElement(TransformableElement *)));
	connect(element, SIGNAL(moveForward(TransformableElement *)), this, SLOT(onMoveForwardElement(TransformableElement *)));
	connect(element, SIGNAL(sendBack(TransformableElement *)), this, SLOT(onSendBackElement(TransformableElement *)));
}

void TransformableTool::disconnectElement(TransformableElement *element)
{
	disconnect(element, SIGNAL(changing(bool)), this, SLOT(onChangingSession(bool)));
	
	disconnect(element, SIGNAL(mouseEnter(TransformableElement *)), this, SLOT(onElementMouseEnter(TransformableElement *)));
	disconnect(element, SIGNAL(mouseMove(TransformableElement *)), this, SLOT(onElementMouseMove(TransformableElement *)));
	disconnect(element, SIGNAL(mouseLeave(TransformableElement *)), this, SLOT(onElementMouseLeave(TransformableElement *)));
	
	disconnect(element, SIGNAL(moved(TransformableElement *, const QPointF &)), this, SLOT(onElementMoved(TransformableElement *, const QPointF &)));
	disconnect(element, SIGNAL(remove(DrawingElement *)), this, SLOT(onRemoveElement(DrawingElement *)));
	disconnect(element, SIGNAL(resizeRotate(TransformableElement *, qreal, qreal)), this, SLOT(onResizeRotateElement(TransformableElement *, qreal, qreal)));
	disconnect(element, SIGNAL(reflect(TransformableElement *)), this, SLOT(onReflectElement(TransformableElement *)));

	disconnect(element, SIGNAL(bringFront(TransformableElement *)), this, SLOT(onBringFrontElement(TransformableElement *)));
	disconnect(element, SIGNAL(moveBackward(TransformableElement *)), this, SLOT(onMoveBackwardElement(TransformableElement *)));
	disconnect(element, SIGNAL(moveForward(TransformableElement *)), this, SLOT(onMoveForwardElement(TransformableElement *)));
	disconnect(element, SIGNAL(sendBack(TransformableElement *)), this, SLOT(onSendBackElement(TransformableElement *)));
}

void TransformableTool::onChangingSession(bool started)
{
	if (started)
		sessionNumber ++;

	sessionStarted = started;
}

void TransformableTool::onElementMouseEnter(TransformableElement *element)
{
	if (!element->isSelected() && element->isSelectable())
	{
		DrawingElement *currentSelection = scene->getSelectedElement();

		if (currentSelection != NULL && !currentSelection->isDeselectable())
			return;

		element->setSelected(true);
	}
}

void TransformableTool::onElementMouseMove(TransformableElement *element)
{
	if (!element->isSelected() && element->isSelectable())
	{
		DrawingElement *currentSelection = scene->getSelectedElement();

		if (currentSelection != NULL && !currentSelection->isDeselectable())
			return;

		element->setSelected(true);
	}
}

void TransformableTool::onElementMouseLeave(TransformableElement *element)
{
	if (element->isDeselectable())
		element->setSelected(false);
}

void TransformableTool::onElementMoved(TransformableElement *element, const QPointF &oldPos)
{
	if (!sessionStarted)
		return;

	doCommand(new MoveElementCommand(element, oldPos, sessionNumber));
}

void TransformableTool::onResizeRotateElement(TransformableElement *element, qreal diffScale, qreal diffAngle)
{	
	if (!sessionStarted)
		return;

	doCommand(new ResizeRotateElementCommand(element, diffScale, diffAngle, sessionNumber));
}

void TransformableTool::onReflectElement(TransformableElement *element)
{
	doCommand(new ReflectElementCommand(element));
}

void TransformableTool::onBringFrontElement(TransformableElement *element)
{
	qreal newZOrder = scene->getNewZOrder(element, EditorScene::BringFront);
	doCommand(new ChangeElementZOrderCommand(element, newZOrder));
}

void TransformableTool::onMoveBackwardElement(TransformableElement *element)
{
	qreal newZOrder = scene->getNewZOrder(element, EditorScene::MoveBackward);
	doCommand(new ChangeElementZOrderCommand(element, newZOrder));
}

void TransformableTool::onMoveForwardElement(TransformableElement *element)
{
	qreal newZOrder = scene->getNewZOrder(element, EditorScene::MoveForward);
	doCommand(new ChangeElementZOrderCommand(element, newZOrder));
}

void TransformableTool::onSendBackElement(TransformableElement *element)
{
	qreal newZOrder = scene->getNewZOrder(element, EditorScene::SendBack);
	doCommand(new ChangeElementZOrderCommand(element, newZOrder));
}

}