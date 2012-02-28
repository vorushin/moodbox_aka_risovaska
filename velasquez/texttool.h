#ifndef TEXTTOOL_H
#define TEXTTOOL_H

#include <QRectF>

#include "transformabletool.h"

class QAction;

namespace Velasquez
{

class TextElement;
class TextCursorPointer;

// Tool for text elements
class TextTool : public TransformableTool
{
	Q_OBJECT

public:
	TextTool(QObject *parent);
	virtual ~TextTool();

	virtual void setScene(EditorScene *scene);
	virtual void setUndoStack(QUndoStack *undoStack);

	virtual bool useKeySequence(const QKeySequence &sequence);

	virtual qint32 getElementType() const;
	virtual QStringList getFileExtensions() const;

	virtual bool canCreate(QGraphicsSceneMouseEvent *mouseEvent) const;
	virtual bool canCreate(QKeyEvent *keyEvent) const;
	virtual bool canCreate(const QMimeData *data) const;

	virtual void createElement(QGraphicsSceneMouseEvent *mouseEvent);
	virtual void createElement(QKeyEvent *keyEvent);
	virtual void createElement(const QMimeData *data, const QPointF &pos = QPointF());

protected:
	virtual void setupNewElement(TransformableElement *element, const QPointF &pos);

	virtual void connectElement(TransformableElement *element);
	virtual void disconnectElement(TransformableElement *element);

	inline bool isEditingElement() const { return editingElement != NULL; };

protected slots:
	void onEditingStarted(TextElement *element);
	void onEditingFinished(TextElement *element);

	void onTextChanged(TextElement *element);
	void onTextUndoAdded(TextElement *element);

private:
	TextElement *editingElement;
	int editingElementRevision;
	
	// Element environment
	TextCursorPointer *cursorPointer;
	bool cursorPointerOwner;
	QAction *commonUndoAction, *commonRedoAction;

	// Find an empty place for new text
	QPointF getNewTextPos() const;

	// Currently editing element
	void setEditingElement(TextElement *element);

private slots:
	// Clear editing element if it was removed
	void onEditingElementDestroyed(QObject *element);

	// Clear cursor pointer if it was removed
	void onCursorPointerDestroyed();

	// Undo/redo state changed
	void onCanUndoChanged(bool canUndo);
	void onCanRedoChanged(bool canRedo);
};

// Class for delayed remove of empty text elements
class EmptyElementRemover : public QObject
{
	Q_OBJECT

public:
	EmptyElementRemover(TextElement *element, TextTool *tool);
	virtual ~EmptyElementRemover();

private:
	TextElement *element;
	TextTool *tool;
	bool destroyed;

private slots:
	void onDestroyed();
};

}

#endif // TEXTTOOL_H
