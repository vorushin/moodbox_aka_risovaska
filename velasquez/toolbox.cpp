#include "toolbox.h"

#include <QUndoStack>
#include <QGraphicsSceneMouseEvent>
#include <QKeyEvent>
#include <QMimeData>
#include <QApplication>
#include <QClipboard>
#include <QGraphicsView>

#include "editorscene.h"
#include "drawingelement.h"
#include "drawingtool.h"
#include "undocommands.h"
#include "debug.h"

namespace Velasquez
{

ToolBox::ToolBox(QObject *parent)
	: QObject(parent), currentTool(NULL), dataHandlingTool(NULL), beforeAlternativeTool(NULL),
	  usingAlternativeTool(false), scene(NULL), undoStack(NULL)
{
}

void ToolBox::addTool(DrawingTool *tool)
{
	tools[tool->getElementType()] = tool;

	tool->setScene(scene);
	tool->setUndoStack(undoStack);
}

void ToolBox::removeTool(qint32 elementType)
{
	DrawingTool *tool = getTool(elementType);
	
	if (tool == NULL)
		return;

	tools.remove(elementType);

	if (currentTool == tool)
		currentTool = NULL;
}

DrawingTool *ToolBox::getTool(qint32 elementType)
{
	return (tools.contains(elementType)) ? tools[elementType] : NULL;
}

void ToolBox::setCurrentTool(qint32 elementType, bool reinit)
{
	DrawingTool *tool = getTool(elementType);

	if (tool == NULL || tool == currentTool && !reinit)
		return;

	if (currentTool != NULL)
	{
		currentTool->clearSelection();
		
		if (scene != NULL)
			scene->clearFocus();
	}

	currentTool = tool;

	if (scene != NULL)
	{
		if (tool->hasCursor())
			scene->setCursor(tool->getDefaultCursor());
		else
			scene->unsetCursor();
	}

	emit toolActivated(tool);
}
	
DrawingTool *ToolBox::getCurrentTool()
{
	return currentTool;
}

void ToolBox::addAlternativeTool(qint32 toolElementType, qint32 alternativeElementType)
{
	alternativeTools[toolElementType] = alternativeElementType;
}

void ToolBox::removeAlternativeTool(qint32 toolElementType)
{
	alternativeTools.remove(toolElementType);
}

qint32 ToolBox::getAlternativeTool(qint32 toolElementType)
{
	return (alternativeTools.contains(toolElementType)) ? alternativeTools[toolElementType] : -1;
}

DrawingTool *ToolBox::getAlternativeToCurrentTool()
{
	qint32 currentElementType = currentTool->getElementType();
	return (getAlternativeTool(currentElementType) > 0) ? getTool(getAlternativeTool(currentTool->getElementType())) : NULL;
}

ToolBox::EventHandleResult ToolBox::handleInputEvent(QEvent *event)
{
	QGraphicsSceneMouseEvent *mouseEvent;
	QKeyEvent *keyEvent;
	Qt::KeyboardModifiers modifiers = QApplication::keyboardModifiers();

	EventHandleResult result = ToolBox::Skipped;

    switch (event->type()) 
	{	    
		case QEvent::GraphicsSceneMousePress:
		case QEvent::GraphicsSceneMouseDoubleClick:
		case QEvent::GraphicsSceneMouseRelease:
		case QEvent::GraphicsSceneMouseMove:

			mouseEvent = static_cast<QGraphicsSceneMouseEvent *>(event);
			modifiers = mouseEvent->modifiers();

			if (currentTool->canCreate(mouseEvent))
			{
				currentTool->createElement(mouseEvent);

				result = Accepted;
			}

			break;

		case QEvent::KeyPress:
		case QEvent::KeyRelease:

			keyEvent = static_cast<QKeyEvent *>(event);
			
			if (currentTool->canCreate(keyEvent))
			{
				currentTool->createElement(keyEvent);

				result = Accepted;
			}
			
			if (processKeyboardEvent(keyEvent))
				result = Accepted;
			break;
	
		default:
			break;
	}

	// Set alternative tool if Alt was pressed in any unconditional way
	if ( (modifiers & Qt::AltModifier) && !isUsingAlternativeTool() )
	{
		startUsingAlternativeTool();
	}

	// Clear alternative tool if Alt was released in any unconditional way
	if ( !(modifiers & Qt::AltModifier) && isUsingAlternativeTool() )
	{
		stopUsingAlternativeTool();
	}

	return result;
}

ToolBox::EventHandleResult ToolBox::handleDragEnterEvent(QGraphicsSceneDragDropEvent *event)
{
	dataHandlingTool = findToolForData(event->mimeData());
	
	return (dataHandlingTool != NULL) ? Accepted : Ignored;
}

ToolBox::EventHandleResult ToolBox::handleDragMoveEvent(QGraphicsSceneDragDropEvent *event)
{
	Q_UNUSED(event)
	
	return (dataHandlingTool != NULL) ? Accepted : Ignored;
}

ToolBox::EventHandleResult ToolBox::handleDragDropEvent(QGraphicsSceneDragDropEvent *event)
{
    if (dataHandlingTool != NULL)
	{
		dataHandlingTool->createElement(event->mimeData(), event->scenePos());
		dataHandlingTool = NULL;

		return Accepted;
	}

	return Ignored;
}

DrawingTool *ToolBox::findToolForData(const QMimeData *data) const
{
	foreach (DrawingTool *tool, tools.values())
	{
		if (tool->canCreate(data))
			return tool;
	}

	return NULL;
}

DrawingTool *ToolBox::findToolWithSelection() const
{
	if (currentTool != NULL && currentTool->hasSelection())
		return currentTool;

	foreach (DrawingTool *tool, tools.values())
	{
		if (tool == currentTool)
			continue;

		if (tool->hasSelection())
			return tool;
	}

	return NULL;
}

bool ToolBox::sendKeySequenceToTools(const QKeySequence &sequence)
{
	if (currentTool != NULL && currentTool->useKeySequence(sequence))
		return true;

	foreach (DrawingTool *tool, tools.values())
	{
		if (tool == currentTool)
			continue;

		if (tool->useKeySequence(sequence))
			return true;
	}

	return false;
}

bool ToolBox::processKeyboardEvent(const QKeyEvent *keyEvent)
{
	if (keyEvent->type() == QEvent::KeyRelease)
		return false;

	// Deleting elements, if possible
	if (keyEvent->matches(QKeySequence::Delete))
	{	
		if (!sendKeySequenceToTools(QKeySequence(QKeySequence::Delete)))
		{
			DrawingTool *toolWithSelection = findToolWithSelection();

			if (toolWithSelection != NULL)
			{
				toolWithSelection ->deleteSelected();

				return true;
			}
			else
				return false;
		}
	}

	if (undoStack == NULL)
		return false;

	// Undo
	if (keyEvent->matches(QKeySequence::Undo))
	{
		if (!sendKeySequenceToTools(QKeySequence(QKeySequence::Undo)))
		{
			undoStack->undo();

			return true;
		}
	}

	// Redo
	if (keyEvent->matches(QKeySequence::Redo))
	{
		if (!sendKeySequenceToTools(QKeySequence(QKeySequence::Redo)))
		{
			undoStack->redo();

			return true;
		}
	}

	// Copy
	if (keyEvent->matches(QKeySequence::Copy))
	{		
		if (!sendKeySequenceToTools(QKeySequence(QKeySequence::Copy)))
		{
			emit copyRequest();

			return true;
		}
	}

	// Paste
	if (keyEvent->matches(QKeySequence::Paste))
	{		
		if (!sendKeySequenceToTools(QKeySequence(QKeySequence::Paste)))
		{
			if (canPasteFromClipboard())
			{
				emit pasteRequest();
			}

			return true;
		}
	}

	return false;
}

bool ToolBox::startUsingAlternativeTool()
{
	if (isUsingAlternativeTool())
		stopUsingAlternativeTool();

	qint32 alternativeToolType = getAlternativeTool(currentTool->getElementType());

	if (alternativeToolType > 0)
	{
		beforeAlternativeTool = currentTool;
		setCurrentTool(alternativeToolType);

		usingAlternativeTool = true;

		return true;
	}

	return false;
}

bool ToolBox::stopUsingAlternativeTool()
{
	if (usingAlternativeTool)
	{
		setCurrentTool(beforeAlternativeTool->getElementType());
		
		// Make sure nothing left selected
		foreach(DrawingTool *tool, tools.values())
		{
			if (tool != beforeAlternativeTool)
			{
				tool->clearSelectionFocus();
				tool->clearSelection();
			}
		}

		beforeAlternativeTool = NULL;

		usingAlternativeTool = false;

		return true;
	}

	return false;
}

void ToolBox::paste(const QMimeData *data)
{
	DrawingTool *tool = findToolForData(data);

	if (tool == NULL)
		return;

	tool->createElement(data);

	if (tool != getCurrentTool())
	{
		tool->clearSelectionFocus();
		tool->clearSelection();
	}
}
	
bool ToolBox::canPasteFromClipboard() const
{
	return (findToolForData(QApplication::clipboard()->mimeData()) != NULL);
}

void ToolBox::setScene(EditorScene *scene)
{
	if (this->scene == scene)
		return;

	if (this->scene != NULL)
		disconnect(this->scene, SIGNAL(cleared()), this, SLOT(onSceneCleared()));

	this->scene = scene;

	if (scene != NULL)
		connect(scene, SIGNAL(cleared()), this, SLOT(onSceneCleared()));

	foreach (DrawingTool *tool, tools.values())
		tool->setScene(scene);
}

void ToolBox::setUndoStack(QUndoStack *undoStack)
{
	if (this->undoStack == undoStack)
		return;

	this->undoStack = undoStack;

	foreach (DrawingTool *tool, tools.values())
		tool->setUndoStack(undoStack);
}

void ToolBox::deleteAllElements()
{
	bool hasSomethingToDelete = false;

	foreach (DrawingTool *tool, tools.values())
		if (tool->hasDeletableElements())
		{
			hasSomethingToDelete = true;
			break;
		}

	if (!hasSomethingToDelete)
		return;

	if (undoStack)
		undoStack->beginMacro(tr(DELETE_COMMAND_NAME));

	foreach (DrawingTool *tool, tools.values())
		tool->deleteAll();

	if (undoStack)
		undoStack->endMacro();
}

void ToolBox::copyImageToClipboard()
{
	if (scene == NULL)
		return;

	QApplication::clipboard()->setImage(scene->renderToImage());
}

void ToolBox::pasteFromClipboard()
{
	if (canPasteFromClipboard())
		paste(QApplication::clipboard()->mimeData());
}

void ToolBox::onSceneCleared()
{
	undoStack->clear();
}

}
