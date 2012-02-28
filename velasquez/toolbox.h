#ifndef TOOLBOX_H
#define TOOLBOX_H

#include <QObject>
#include <QHash>
#include <QKeySequence>

class QUndoStack;
class QMimeData;
class QGraphicsSceneDragDropEvent;
class QKeyEvent;

namespace Velasquez
{

class DrawingTool;
class EditorScene;

// Class for keeping and managing of drawing tools
class ToolBox : public QObject
{
	Q_OBJECT

public:
	enum EventHandleResult {Accepted, Ignored, Skipped};

public:
	ToolBox(QObject *parent);

	// Add/Remove/Get tool
	void addTool(DrawingTool *tool);
	void removeTool(qint32 elementType);
	DrawingTool *getTool(qint32 elementType);

	// Current tool
	void setCurrentTool(qint32 elementType, bool reinit = false);	
	DrawingTool *getCurrentTool();

	// Alternative tool
	void addAlternativeTool(qint32 toolElementType, qint32 alternativeElementType);
	void removeAlternativeTool(qint32 toolElementType);
	qint32 getAlternativeTool(qint32 toolElementType);
	DrawingTool *getAlternativeToCurrentTool();
	inline bool isUsingAlternativeTool() const { return usingAlternativeTool; };

	// Events
	EventHandleResult handleInputEvent(QEvent *event);
	
	EventHandleResult handleDragEnterEvent(QGraphicsSceneDragDropEvent *event);
	EventHandleResult handleDragMoveEvent(QGraphicsSceneDragDropEvent *event);
	EventHandleResult handleDragDropEvent(QGraphicsSceneDragDropEvent *event);

	// Data
	void paste(const QMimeData *data);
	bool canPasteFromClipboard() const;

	// Common functions
	void setScene(EditorScene *scene);
	void setUndoStack(QUndoStack *undoStack);

	void deleteAllElements();
	
public slots:
	void copyImageToClipboard();
	void pasteFromClipboard();
	void onSceneCleared();

signals:
	void toolActivated(DrawingTool *tool);

	void copyRequest();
	void pasteRequest();

protected:
	DrawingTool *findToolForData(const QMimeData *data) const;
	DrawingTool *findToolWithSelection() const;

	bool sendKeySequenceToTools(const QKeySequence &sequence);

	bool processKeyboardEvent(const QKeyEvent *keyEvent);

	bool startUsingAlternativeTool();
	bool stopUsingAlternativeTool();

private:
	QHash <qint32, DrawingTool*> tools;
	QHash <qint32, qint32> alternativeTools;

	DrawingTool *currentTool, *dataHandlingTool, *beforeAlternativeTool;
	
	bool usingAlternativeTool;

	EditorScene *scene;
	QUndoStack *undoStack;
};

}

#endif // TOOLBOX_H
