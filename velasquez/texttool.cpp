#include "texttool.h"

#include <QGraphicsSceneMouseEvent>
#include <QKeyEvent>
#include <QMimeData>
#include <QUrl>
#include <QAction>
#include <QKeySequence>

#include "textelement.h"
#include "editorscene.h"
#include "textcursorpointer.h"
#include "undocommands.h"

#include "vcommon.h"
#include "debug.h"

namespace Velasquez
{

// Comment to hide debug
//#define SHOW_TEXTTOOL_DEBUG					true

#if (defined(UDEBUG)) && (defined(SHOW_TEXTTOOL_DEBUG))
#define TEXTTOOLDEBUG(x)	QDEBUG(x)
#else
#define TEXTTOOLDEBUG(x)
#endif

// EmptyElementRemover class
EmptyElementRemover::EmptyElementRemover(TextElement *element, TextTool *tool) : QObject()
{
	this->element = element;
	this->tool = tool;
	destroyed = false;

	connect(element, SIGNAL(destroyed()), this, SLOT(onDestroyed()));
}

EmptyElementRemover::~EmptyElementRemover()
{	
	if (!destroyed)
	{
		if (element->scene() != NULL)
			tool->destroyElement(element);
	}
}

void EmptyElementRemover::onDestroyed()
{
	destroyed = true;
}

// TextTool class
TextTool::TextTool(QObject *parent)
	: TransformableTool(parent), editingElement(NULL), cursorPointer(NULL), cursorPointerOwner(false), commonUndoAction(NULL), commonRedoAction(NULL)
{
}

TextTool::~TextTool()
{
	if (cursorPointerOwner)
		delete cursorPointer;
}

void TextTool::setScene(EditorScene *scene)
{
	if (scene != this->scene)
	{
		if (cursorPointer != NULL && this->scene != NULL)
		{
			this->scene->removeItem(cursorPointer);
			cursorPointerOwner = true;
		}

		if (scene != NULL)
		{
			if (cursorPointer == NULL)
			{
				cursorPointer = new TextCursorPointer();
				connect(cursorPointer, SIGNAL(destroyed()), this, SLOT(onCursorPointerDestroyed()));
			}
			
			scene->addItem(cursorPointer);
			scene->addDecorationType(TextCursorPointer::Type, EditorScene::ItemDecoration);
			cursorPointerOwner = false;
		}
	}

	TransformableTool::setScene(scene);
}

void TextTool::setUndoStack(QUndoStack *undoStack)
{
	if (undoStack != this->undoStack)
	{
		// Disconnect old undo
		if (commonUndoAction != NULL && this->undoStack != NULL)
		{
			disconnect(this->undoStack, SIGNAL(canUndoChanged(bool)), this, SLOT(onCanUndoChanged(bool)));
			disconnect(commonUndoAction, SIGNAL(triggered()), this->undoStack, SLOT(undo()));
		}
		
		// Disconnect old redo
		if (commonRedoAction != NULL && this->undoStack != NULL)
		{
			disconnect(this->undoStack, SIGNAL(canRedoChanged(bool)), this, SLOT(onCanRedoChanged(bool)));
			disconnect(commonRedoAction, SIGNAL(triggered()), this->undoStack, SLOT(redo()));
		}
	}

	if (undoStack != NULL)
	{
		if (commonUndoAction == NULL)
			this->commonUndoAction = new QAction(tr(UNDO_MENU_ITEM_TEXT), this);

		if (commonRedoAction == NULL)
			this->commonRedoAction = new QAction(tr(REDO_MENU_ITEM_TEXT), this);

		// Connect new undo
		connect(undoStack, SIGNAL(canUndoChanged(bool)), this, SLOT(onCanUndoChanged(bool)));
		connect(commonUndoAction, SIGNAL(triggered()), undoStack, SLOT(undo()));

		// Connect new redo
		connect(undoStack, SIGNAL(canRedoChanged(bool)), this, SLOT(onCanRedoChanged(bool)));
		connect(commonRedoAction, SIGNAL(triggered()), undoStack, SLOT(redo()));
	}

	TransformableTool::setUndoStack(undoStack);
}

bool TextTool::useKeySequence(const QKeySequence &sequence)
{
	if (isEditingElement())
	{
		// Editing element handles these keys but itself
		return sequence.matches(QKeySequence::Delete) || 
			   sequence.matches(QKeySequence::Undo) || 
			   sequence.matches(QKeySequence::Redo) || 
			   sequence.matches(QKeySequence::Paste) ||
			   sequence.matches(QKeySequence::Copy);
	}

	return false;
}

qint32 TextTool::getElementType() const
{
	return TextElement::Type;
}

QStringList TextTool::getFileExtensions() const
{
	return QStringList();
}

bool TextTool::canCreate(QGraphicsSceneMouseEvent *mouseEvent) const
{
	// If editing - quit at once
	if (isEditingElement())
		return false;

	// Only left mouse press is interesting 
	if ( (mouseEvent->type() != QEvent::GraphicsSceneMousePress && mouseEvent->type() != QEvent::GraphicsSceneMouseDoubleClick) || mouseEvent->button() != Qt::LeftButton )
		return false;

	QPointF currentPos = mouseEvent->scenePos();

	// Cannot create over text item or item decoration
	QList <QGraphicsItem *> testItems = scene->items(currentPos);
	foreach (QGraphicsItem *testItem, testItems)
	{
		if (testItem->type() == getElementType() || scene->isDecorationType(testItem->type(), EditorScene::ItemDecorations))
			return false;
	}

	return true; 
}

bool TextTool::canCreate(QKeyEvent *keyEvent) const
{
	Q_UNUSED(keyEvent)

	return false;
}
	
bool TextTool::canCreate(const QMimeData *data) const
{
	// If editing - quit at once
	if (isEditingElement())
		return false;

	// We want any text if there is no image (this is for images)
	return ( data->hasText() || data->hasHtml() ) && !data->hasImage();
}

void TextTool::createElement(QGraphicsSceneMouseEvent *mouseEvent)
{
	setupNewElement(new TextElement(), mouseEvent->scenePos());
}

void TextTool::createElement(QKeyEvent *keyEvent)
{
	Q_UNUSED(keyEvent)
}

void TextTool::createElement(const QMimeData *data, const QPointF &pos)
{
	bool isHtml = data->hasHtml();
	QString text = (isHtml) ? data->html() : data->text();
	TextElement *element = new TextElement(text, isHtml);
	
	QPointF newPos = getCenteredElementPosition(pos, element);

	setupNewElement(element, newPos);
}

void TextTool::setupNewElement(TransformableElement *element, const QPointF &pos)
{
	TransformableTool::setupNewElement(element, pos);
	
	TextElement *textElement = static_cast <TextElement *> (element);
	textElement->edit();
}

void TextTool::connectElement(TransformableElement *element)
{
	TransformableTool::connectElement(element);

	TextElement *textElement = static_cast <TextElement *> (element);

	connect(textElement, SIGNAL(editingStarted(TextElement *)), this, SLOT(onEditingStarted(TextElement *)));
	connect(textElement, SIGNAL(editingFinished(TextElement *)), this, SLOT(onEditingFinished(TextElement *)));
	connect(textElement, SIGNAL(textChanged(TextElement *)), this, SLOT(onTextChanged(TextElement *)));
	connect(textElement, SIGNAL(textUndoAdded(TextElement *)), this, SLOT(onTextUndoAdded(TextElement *)));

	textElement->setCommonUndoAction(commonUndoAction);
	textElement->setCommonRedoAction(commonRedoAction);
}

void TextTool::disconnectElement(TransformableElement *element)
{
	TransformableTool::disconnectElement(element);

	TextElement *textElement = static_cast <TextElement *> (element);

	disconnect(textElement, SIGNAL(editingStarted(TextElement *)), this, SLOT(onEditingStarted(TextElement *)));
	disconnect(textElement, SIGNAL(editingFinished(TextElement *)), this, SLOT(onEditingFinished(TextElement *)));
	disconnect(textElement, SIGNAL(textChanged(TextElement *)), this, SLOT(onTextChanged(TextElement *)));
	disconnect(textElement, SIGNAL(textUndoAdded(TextElement *)), this, SLOT(onTextUndoAdded(TextElement *)));

	textElement->setCommonUndoAction(NULL);
	textElement->setCommonRedoAction(NULL);
}

void TextTool::onEditingStarted(TextElement *element)
{
	setEditingElement(element);
}

void TextTool::onEditingFinished(TextElement *element)
{
	if (isEditingElement())
	{
		if (element == editingElement) 
		{
			setEditingElement(NULL);
            element->setSelected(false);
        }

		if (element->isEmpty())
		{
			TEXTTOOLDEBUG("posted empty text removal");
			// Remove later to make sure all events were processed
			(new EmptyElementRemover(element, this))->deleteLater();
		}
	}
}

void TextTool::onTextChanged(TextElement *element)
{
	if (element != getSelected())
		return;

	if (editingElementRevision != element->revision())
	{
		editingElementRevision = element->revision();
		TEXTTOOLDEBUG("Text element version changed to " << editingElementRevision);
	}
}

void TextTool::onTextUndoAdded(TextElement *element)
{
	if (element != getSelected())
		return;

	doCommand(new TextUndoCommand(element, editingElementRevision));
}
	
QPointF TextTool::getNewTextPos() const
{
	QRectF sceneRect = scene->sceneRect();
	QPointF pos(sceneRect.left(), sceneRect.top());

	// Seach 
	for ( qreal y = sceneRect.top(); y < sceneRect.bottom(); )
	{
		pos.setY(y);
		
		QGraphicsItem *item = scene->itemAtFiltered(pos);
		
		if (item == NULL)
			return pos;
		else
			y = item->sceneBoundingRect().bottom() + 1;
	}

	// Give up and use 0
	return QPointF(sceneRect.left(), sceneRect.top());
}

void TextTool::setEditingElement(TextElement *element)
{
	if (editingElement != NULL)
	{   
		disconnect(editingElement, SIGNAL(destroyed(QObject *)), this, SLOT(onEditingElementDestroyed(QObject *)));
		disconnect(editingElement, SIGNAL(cursorMoved(TextElement *)), cursorPointer, SLOT(pointToCursor(TextElement *)));
		
		cursorPointer->setActive(false);
	}

	editingElement = element;

	if (element != NULL)
	{
		connect(editingElement, SIGNAL(destroyed(QObject *)), this, SLOT(onEditingElementDestroyed(QObject *)));
		connect(editingElement, SIGNAL(cursorMoved(TextElement *)), cursorPointer, SLOT(pointToCursor(TextElement *)));
		
		cursorPointer->setActive(true);
		editingElementRevision = element->revision();
		
		if (undoStack != NULL)
		{
			onCanUndoChanged(undoStack->canUndo());
			onCanRedoChanged(undoStack->canRedo());
		}
	}
}

void TextTool::onEditingElementDestroyed(QObject *element)
{
	if (element == editingElement)
	{
		if (cursorPointer != NULL)
			cursorPointer->setActive(false);

		editingElement = NULL;
	}
}

void TextTool::onCursorPointerDestroyed()
{
	cursorPointer = NULL;
}

void TextTool::onCanUndoChanged(bool canUndo)
{
	bool enable = canUndo;
	
	if (canUndo)
	{
		// Enable undo if we are on text undo command & element is the same
		const QUndoCommand *command = undoStack->command(undoStack->index() - 1);
		
		if (command->id() == TextUndoCommand::Id)
		{
			const TextUndoCommand *undoCommand = static_cast<const TextUndoCommand*>(command);
			
			enable = (undoCommand->getElement() == editingElement);
		}
		else
			enable = false;
	}
	
	commonUndoAction->setEnabled(enable);
}

void TextTool::onCanRedoChanged(bool canRedo)
{
	bool enable = canRedo;

	if (canRedo)
	{
		// Enable redo if next is text undo command & element is the same
		const QUndoCommand *command = undoStack->command(undoStack->index());
		
		if (command->id() == TextUndoCommand::Id)
		{
			const TextUndoCommand *undoCommand = static_cast<const TextUndoCommand*>(command);
			
			enable = (undoCommand->getElement() == editingElement);
		}
		else
			enable = false;		
	}

	commonRedoAction->setEnabled(enable);
}

}