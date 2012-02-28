#ifndef UNDOCOMMANDS_H
#define UNDOCOMMANDS_H

#include <QUndoCommand>
#include <QVariant>
#include <QPointF>

namespace Velasquez
{

class DrawingElement;
class TransformableElement;
class TextElement;

#define ADD_COMMAND_NAME			"AddCommand"
#define DELETE_COMMAND_NAME			"DeleteCommand"
#define SETTING_COMMAND_NAME		"SettingCommand"
#define MOVE_COMMAND_NAME			"MoveCommand"
#define RESIZEROTATE_COMMAND_NAME	"ResizeRotateCommand"
#define REFLECT_COMMAND_NAME		"ReflectCommand"
#define ZORDER_COMMAND_NAME			"ZOrderCommand"
#define TEXTUNDO_COMMAND_NAME		"TextUndoCommand"

class EditorScene;

// Basig element action command
class ElementActionCommand : public QUndoCommand
{
public:
	ElementActionCommand(DrawingElement *element, QUndoCommand *parent = 0);

	inline DrawingElement *getElement() const { return this->element; };

protected:
	DrawingElement *element;
};

// Add element command
class AddElementCommand : public ElementActionCommand
{
public:
	enum {Id = 1};

	AddElementCommand(DrawingElement *element, EditorScene *scene, QUndoCommand *parent = 0);
	virtual ~AddElementCommand();
	
	inline virtual int id() const { return Id; };

	virtual void undo();
    virtual void redo();

private:
	EditorScene *scene;
	
	bool elementOwner;
};

// Delete element command
class DeleteElementCommand : public ElementActionCommand
{
public:
	enum {Id = 2};

	DeleteElementCommand(DrawingElement* element, EditorScene *scene, QUndoCommand *parent = 0);
	virtual ~DeleteElementCommand();
	
	inline virtual int id() const { return Id; };

	virtual void undo();
    virtual void redo();

private:
	EditorScene *scene;

	bool elementOwner;
};

// Element setting change command
class ChangeElementSettingCommand : public ElementActionCommand
{
 public:
	 enum {Id = 3};

     ChangeElementSettingCommand(DrawingElement* element, qint32 settingId, const QVariant &newValue, QUndoCommand *parent = 0);

	 inline virtual int id() const { return Id; }

     virtual void undo();
     virtual void redo();

 private:
     QVariant oldValue;
     QVariant newValue;

	 qint32 settingId;
};

// Element move command
class MoveElementCommand : public ElementActionCommand
{
 public:
	 enum {Id = 4};

     MoveElementCommand(DrawingElement* element, const QPointF &oldPos, long sessionNumber, QUndoCommand *parent = 0);

	 inline virtual int id() const { return Id; }

     virtual void undo();
     virtual void redo();

	 bool mergeWith(const QUndoCommand *command);

 private:
     QPointF oldPos, newPos;

	 long sessionNumber;
};

// Element resize and rotate command
class ResizeRotateElementCommand : public ElementActionCommand
{
 public:
	 enum {Id = 5};

     ResizeRotateElementCommand(TransformableElement* element, qreal diffScale, qreal diffAngle, long sessionNumber, QUndoCommand *parent = 0);

	 inline virtual int id() const { return Id; }

     virtual void undo();
     virtual void redo();

	 bool mergeWith(const QUndoCommand *command);

 private:
	 TransformableElement* transformableElement;

     qreal oldScale, oldAngle;
	 qreal newScale, newAngle;

	 long sessionNumber;
};

// Reflect element command
class ReflectElementCommand : public ElementActionCommand
{
public:
	enum {Id = 6};

	ReflectElementCommand(TransformableElement* element, QUndoCommand *parent = 0);
	
	inline virtual int id () const { return Id; };

	virtual void undo();
    virtual void redo();

private:
	TransformableElement *transformableElement;
	bool oldReflected, newReflected;
};

class ChangeElementZOrderCommand : public ElementActionCommand
{
public:
	enum {Id = 7};

	ChangeElementZOrderCommand(DrawingElement* element, qreal newZOrder, QUndoCommand *parent = 0);

	inline virtual int id() const { return Id; }

	virtual void undo();
	virtual void redo();

private:
	qreal oldZOrder;
	qreal newZOrder;
};

class TextUndoCommand : public ElementActionCommand
{
public:
	enum {Id = 8};

	TextUndoCommand(TextElement *element, int lastRevision, QUndoCommand *parent = 0);

	inline virtual int id() const { return Id; }

	virtual void undo();
	virtual void redo();

private:
	TextElement *textElement;

	qreal lastRevision;
	qreal currentRevision;
};

}

#endif // UNDOCOMMANDS_H
