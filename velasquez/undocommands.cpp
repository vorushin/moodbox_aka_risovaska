#include "undocommands.h"

#include <QString>

#include "textelement.h"
#include "editorscene.h"
#include "debug.h"

namespace Velasquez
{

//#define SHOW_UNDO_DEBUG						true

// Undo debug diagnostics
#if (defined(UDEBUG) && defined(SHOW_UNDO_DEBUG))

#define UNDODEBUG(x)						QDEBUG(getDebugString(true, x))
#define REDODEBUG(x)						QDEBUG(getDebugString(false, x))

#define UNDODEBUGEX(x, s)					QDEBUG(getDebugString(true, x, s))
#define REDODEBUGEX(x, s)					QDEBUG(getDebugString(false, x, s))

#define UNDO_COMMAND_DEBUG_INFO				"(%1) %2 of element type %3 address %4 message %5"

#define UNDO_INFO_PREFIX					"Undo "
#define REDO_INFO_PREFIX					"Redo "

qint32 undoCounter = 0;

QString getDebugString(bool isUndo, ElementActionCommand *command, const QString &message = QString())
{
	QString prefix = (isUndo) ? UNDO_INFO_PREFIX : REDO_INFO_PREFIX;
	return prefix + QString(UNDO_COMMAND_DEBUG_INFO).arg(QString::number(undoCounter++)).arg(command->text()).arg(QString::number(command->getElement()->type())).arg((long)(command->getElement())).arg(message);
}

#else

#define UNDODEBUG(x)
#define REDODEBUG(x)

#define UNDODEBUGEX(x, s)
#define REDODEBUGEX(x, s)

#endif

// ElementActionCommand class
ElementActionCommand::ElementActionCommand(DrawingElement *element, QUndoCommand *parent)
	: QUndoCommand(parent)
{
	this->element = element;
}

// AddElementCommand class
AddElementCommand::AddElementCommand(DrawingElement *element, EditorScene *scene, QUndoCommand *parent)
	: ElementActionCommand(element, parent), elementOwner(false)
{
	this->scene = scene;

	setText(QString(ADD_COMMAND_NAME));
}

AddElementCommand::~AddElementCommand()
{
	if (elementOwner)
		delete element;
}

void AddElementCommand::undo()
{
	UNDODEBUG(this);

	scene->removeItem(element);

	elementOwner = true;
}

void AddElementCommand::redo()
{
	REDODEBUG(this);

	scene->addItem(element);
	
	elementOwner = false;
}

// DeleteElementCommand class
DeleteElementCommand::DeleteElementCommand(DrawingElement* element, EditorScene *scene, QUndoCommand *parent)
	: ElementActionCommand(element, parent), elementOwner(false)
{
	this->scene = scene;

	setText(QString(DELETE_COMMAND_NAME));
}

DeleteElementCommand::~DeleteElementCommand()
{
	if (elementOwner)		
		delete element;
}

void DeleteElementCommand::undo()
{
	UNDODEBUG(this);

	scene->addItem(element);

	elementOwner = false;
}

void DeleteElementCommand::redo()
{
	REDODEBUG(this);

	scene->removeItem(element);

	elementOwner = true;
}

// ChangeElementSettingCommand class
ChangeElementSettingCommand::ChangeElementSettingCommand(DrawingElement *element, qint32 settingId, const QVariant &newValue, QUndoCommand *parent)
	: ElementActionCommand(element, parent)
{
	this->settingId = settingId;
	this->newValue = newValue;

	this->oldValue = element->getSetting(settingId);

	setText(QString(SETTING_COMMAND_NAME));
}

void ChangeElementSettingCommand::undo()
{
	UNDODEBUG(this);

	element->setSetting(settingId, oldValue);
}

void ChangeElementSettingCommand::redo()
{
	REDODEBUG(this);

	element->setSetting(settingId, newValue);
}

// MoveElementCommand class
MoveElementCommand::MoveElementCommand(DrawingElement* element, const QPointF &oldPos, long sessionNumber, QUndoCommand *parent)
	: ElementActionCommand(element, parent)
{
	this->sessionNumber = sessionNumber;

	this->oldPos = oldPos;
	this->newPos = element->pos();

	setText(QString(MOVE_COMMAND_NAME));
}

void MoveElementCommand::undo()
{
	UNDODEBUG(this);

	element->setPos(oldPos);

	element->scene()->update();
}

void MoveElementCommand::redo()
{
	REDODEBUG(this);

	element->setPos(newPos);

	element->scene()->update();
}

bool MoveElementCommand::mergeWith(const QUndoCommand *command)
{
	const MoveElementCommand *moveCommand = static_cast<const MoveElementCommand *>(command);
    DrawingElement *moveElement = moveCommand->element;

    if (element != moveElement || this->sessionNumber != moveCommand->sessionNumber)
		return false;

    newPos = element->pos();

    return true;
}

// ResizeRotateElementCommand class
ResizeRotateElementCommand::ResizeRotateElementCommand(TransformableElement* element, qreal diffScale, qreal diffAngle, long sessionNumber, QUndoCommand *parent)
	: ElementActionCommand(element, parent)
{
	this->transformableElement = element;
	this->sessionNumber = sessionNumber;

	oldScale = element->getTransformation().getScale();
	oldAngle = element->getTransformation().getAngle();

	newScale = diffScale * oldScale;
	newAngle = diffAngle + oldAngle;

	setText(QString(RESIZEROTATE_COMMAND_NAME));
}

void ResizeRotateElementCommand::undo()
{
	UNDODEBUG(this);

	Transformation t = this->transformableElement->getTransformation();
	t.setScale(oldScale);
	t.setAngle(oldAngle);

	transformableElement->setTransformation(t);
}

void ResizeRotateElementCommand::redo()
{
	REDODEBUG(this);

	Transformation t = this->transformableElement->getTransformation();
	t.setScale(newScale);
	t.setAngle(newAngle);

	transformableElement->setTransformation(t);
}

bool ResizeRotateElementCommand::mergeWith(const QUndoCommand *command)
{
	const ResizeRotateElementCommand *rrCommand = static_cast<const ResizeRotateElementCommand *>(command);
    TransformableElement *rrElement = rrCommand->transformableElement;

    if (transformableElement != rrElement || this->sessionNumber != rrCommand->sessionNumber)
		return false;

    newScale = rrCommand->newScale;
	newAngle = rrCommand->newAngle;

    return true;
}

// ReflectElementCommand class
ReflectElementCommand::ReflectElementCommand(TransformableElement* element, QUndoCommand *parent)
	: ElementActionCommand(element, parent)
{
	this->transformableElement = element;
	
	this->oldReflected = element->getTransformation().isReflected();
	this->newReflected = !oldReflected;	

	setText(QString(REFLECT_COMMAND_NAME));
}

void ReflectElementCommand::undo()
{
	UNDODEBUG(this);

	transformableElement->setTransformationReflected(oldReflected);
}

void ReflectElementCommand::redo()
{
	REDODEBUG(this);

	transformableElement->setTransformationReflected(newReflected);
}

// ChangeElementZOrderCommand class
ChangeElementZOrderCommand::ChangeElementZOrderCommand(DrawingElement* element, qreal newZOrder, QUndoCommand *parent)
	: ElementActionCommand(element, parent)
{
	this->newZOrder = newZOrder;
	this->oldZOrder = element->zValue();

	setText(QString(ZORDER_COMMAND_NAME));
}

void ChangeElementZOrderCommand::undo()
{
	UNDODEBUG(this);

	element->setZValue(oldZOrder);
}

void ChangeElementZOrderCommand::redo()
{
	REDODEBUG(this);

	element->setZValue(newZOrder);
}

// TextUndoCommand class
TextUndoCommand::TextUndoCommand(TextElement* element, int lastRevision, QUndoCommand *parent)
	: ElementActionCommand(element, parent)
{
	this->textElement = element;

	this->lastRevision = lastRevision;
	this->currentRevision = textElement->revision();

	setText(QString(TEXTUNDO_COMMAND_NAME));
}

void TextUndoCommand::undo()
{	
	if (textElement->revision() == currentRevision)
	{
		UNDODEBUG(this);

		textElement->textUndo();
	}
	else
	{
		UNDODEBUGEX(this, QString("versions are different, needed %1 now %2").arg(QString::number(currentRevision)).arg(QString::number(textElement->revision())));
	}
}

void TextUndoCommand::redo()
{
	if (textElement->revision() == lastRevision)
	{
		REDODEBUG(this);
		textElement->textRedo();
	}
	else
	{
		REDODEBUGEX(this, QString("versions are different, needed %1 now %2").arg(QString::number(lastRevision)).arg(QString::number(textElement->revision())));
	}

}

}