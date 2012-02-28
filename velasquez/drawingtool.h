#ifndef DRAWINGTOOL_H
#define DRAWINGTOOL_H

#include <QObject>
#include <QPointF>
#include <QCursor>

class QUndoStack;
class QGraphicsSceneMouseEvent;
class QKeyEvent;
class QMimeData;
class QKeySequence;

namespace Velasquez
{

class EditorScene;
class DrawingElement;
class SettingsProvider;
class ElementActionCommand;
	
// New element indent for drag and drop
#define NEW_ELEMENT_INDENT		10
	
// Base class for drawing tools
// Responsible for creation, synchronization and supervison under drawing elements
class DrawingTool : public QObject
{
	Q_OBJECT

public:
	DrawingTool(QObject *parent);

	// Scene
	virtual void setScene(EditorScene *scene);

	// Undo stack, if specified all undoable actions executed throught it
	virtual void setUndoStack(QUndoStack *undoStack);

	// Cursor operations
	virtual bool hasCursor() const;
	virtual QCursor getDefaultCursor() const;

	// Keyboard events processing
	virtual bool useKeySequence(const QKeySequence &sequence);

	// Settings provider to forward settings to elements
	virtual void setSettingsProvider(SettingsProvider *settings);
	inline SettingsProvider* getSettingsProvider() const { return this->settings; };

	// Elements type
	virtual qint32 getElementType() const = 0;
	virtual QStringList getFileExtensions() const = 0;

	// Creating, adding, removing
	virtual bool canCreate(QGraphicsSceneMouseEvent *mouseEvent) const = 0;
	virtual bool canCreate(QKeyEvent *keyEvent) const = 0;
	virtual bool canCreate(const QMimeData *data) const = 0;

	virtual void createElement(QGraphicsSceneMouseEvent *mouseEvent) = 0;
	virtual void createElement(QKeyEvent *keyEvent) = 0;
	virtual void createElement(const QMimeData *data, const QPointF &pos = QPointF()) = 0;

	// Destroy created element
	virtual bool destroyElement(DrawingElement *element);
	
	// Add existing element, insert it to scene if needed
	virtual bool addElement(DrawingElement *element);
	
	// Selection
	inline DrawingElement* getSelected() const { return this->selected; };
	inline bool hasSelection() const { return selected != NULL; };
	
	void clearSelection();
	void clearSelectionFocus();

	// Deletion
	virtual bool hasDeletableElements() const;
	virtual void deleteSelected();
	virtual void deleteAll();

signals:
	void elementSelected(DrawingElement *element);
	void selectionChanged(DrawingElement *oldSelected, DrawingElement *newSelected);

protected:
	EditorScene *scene;
	QUndoStack *undoStack;
	SettingsProvider *settings;

	DrawingElement *selected;

	// Settings operations
	// Update all element settings
	void updateElementSettings(DrawingElement *element);
	// Update settings of selected element
	void updateSelectedSettings();

	// Setup new element
	virtual void setupNewElement(DrawingElement *element, const QPointF &pos);
	
	// Extract element from this tool
	virtual void extractElement(DrawingElement *element);

	// Get all elements of current type from scene
	virtual QList <DrawingElement *> getElementsFromScene() const;

	// Element position operations
	// Get centered position for the element on the scene, pos is adusted to item center if specified
	QPointF getCenteredElementPosition(const QPointF &initialPos = QPointF(), DrawingElement *centerElement = NULL) const;
	// Get next element position (usable when creating several elements)
	QPointF getNextElementPosition(const QPointF &initialPos) const;

	// Commands and macros operations
	// Run command (if stack is not specified) or put to stack
	virtual void doCommand(ElementActionCommand *command);
	// Run list of commands as a macro
	void doMacro(const QList <ElementActionCommand *> &commands, const QString &name = QString());

	// Mime and file type operations
	// Check MIME data for correct files
	virtual bool isMimeSupported(const QMimeData *data) const;
	// Extract supported files from Mime data
	virtual QStringList getSupportedFilesFromMime(const QMimeData *data) const;

	// Check whether file is supported
	static bool isFileSupported(const QString &fileName, const QStringList &extensions);

protected slots:
	// Re-load selected item
	virtual void updateSelection();

	// Update setting for selected item
	virtual void onSettingChanged(qint32 id);

	// Remove element via undo
	void onRemoveElement(DrawingElement *element);

private:
	void connectSceneSelectionUpdate();
	void disconnectSceneSelectionUpdate();
};

}

#endif // DRAWINGTOOL_H
