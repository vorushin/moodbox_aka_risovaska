#ifndef EDITORSCENE_H
#define EDITORSCENE_H

#include <QGraphicsScene>
#include "metainfoprovider.h"

#include <QImage>

#include <QFlags>
#include <QHash>

namespace Velasquez
{

class ToolBox;
class SceneCursorLayer;
class DrawingElement;

class BackgroundElement;

// Z-order signle step
#define ZORDER_STEP								1.0

// Metainfo
#define METAINFO_SCENE_TITLE					"Scene"
#define METAINFO_SCENEELEMENTS_TAGNAME			"ElementTypes"

// Class for rendering elements and event forwarding to toolbox
class EditorScene : public QGraphicsScene, public MetaInfoProvider
{
	Q_OBJECT

public:
	// Z-order operations
	enum ZOrderAction { SendBack, MoveBackward, MoveForward, BringFront };

	// Decoration types
	enum DecorationType { SceneDecoration, ItemDecoration };

	// Decoration filter
	enum DecorationFilterFlag { SceneDecorations = 0x01, ItemDecorations = 0x02, AllDecorations = 0x03 };
	Q_DECLARE_FLAGS(DecorationFilter, DecorationFilterFlag)

public:	
	EditorScene(QObject *parent);

	virtual void addItem(QGraphicsItem *item);
	virtual void removeItem(QGraphicsItem *item);

	// Remove all items from scene
	virtual void clear();

	// Change scene background
	virtual void setBackgroundColor(const QColor &color);
	virtual QColor getBackgroundColor();

	BackgroundElement *getBackground() const { return background; };
	bool hasBackground() const { return background != NULL; };
	void updateBackground(BackgroundElement *background);
	
	// Add element(s) to scene
	void addElement(DrawingElement *element);
	void addElements(QList <DrawingElement *> elements);

	// Get elements
	QList <DrawingElement *> getElements() const;
	QList <qint32> getElementTypesList() const;

	// Get elements and remove them from scene
	QList <DrawingElement *> extractElements();

	// Get scene image
	QImage renderToImage();

	// Toolbox
	void setToolBox(ToolBox *toolBox);
	inline ToolBox * getToolBox() const { return toolBox; };

	// Selected element
	DrawingElement *getSelectedElement() const { return selectedElement; };

	// Z-order
	inline virtual qreal getCurrentZOrder() const { return z; };
	inline virtual qreal getNewZOrder() { return z += ZORDER_STEP; };
	virtual qreal getNewZOrder(const QGraphicsItem *item, ZOrderAction action) const;

	// Filters to ignore miscellaneuos (e.g. decoration) types when working with scene items
	virtual void addDecorationType(qint32 itemType, DecorationType decorationType);
	void addDecorationTypes(QList <qint32> itemTypes, DecorationType decorationType);
	virtual void removeDecorationType(qint32 itemType);
	virtual bool isDecorationType(qint32 itemType, DecorationFilter decorationFilter = AllDecorations) const;

	// Scene cursor
	void setCursor(const QCursor &cursor);
	void unsetCursor();

	// Items access
	// Topmost item at specified point but filtered from decoration
	QGraphicsItem *itemAtFiltered(const QPointF &pos, DecorationFilter decorationFilter = AllDecorations) const;	

	// Metainfo
	inline virtual QString getInfoTitle() const { return METAINFO_SCENE_TITLE; };
	virtual QStringList getTagNames() const;
	virtual QString getTagContent(const QString &tagName) const;
	virtual QString getInfoContentAsXml() const;

signals:
	void cleared();
	void backgroundColorChanged(const QColor &color);

	void itemAdded(QGraphicsItem *item);
	void itemRemoved(QGraphicsItem *item);

protected:
	virtual bool event(QEvent *event);

	virtual void dragEnterEvent(QGraphicsSceneDragDropEvent *event);
	virtual void dragMoveEvent(QGraphicsSceneDragDropEvent *event);
	virtual void dropEvent(QGraphicsSceneDragDropEvent *event);

	// Get items with null parent
	virtual QList <QGraphicsItem *> getRootItems() const;

	// Get visible known elements with null parent
	virtual QList <DrawingElement *> getVisibleRootElements() const;

	// Add element to scene after load or add
	virtual void addElementToScene(DrawingElement *element);

	// Z-order
	// Get minimum and maximum z-order on the scene
	virtual void getMinAndMaxZOrder(qreal &minZ, qreal &maxZ) const;

	// Finds next z-order for the item, in both directions
	virtual void getItemNextZOrder(const QGraphicsItem *item, qreal &backward, qreal &forward) const;

private:
	BackgroundElement *background;

	ToolBox *toolBox;
	DrawingElement *selectedElement;

	qreal z;
	SceneCursorLayer *cursorLayerItem;

	bool mouseInitialized;

	QHash <qint32, DecorationType> decorationTypes;

private slots:
	void onSelectionChanged();
};

Q_DECLARE_OPERATORS_FOR_FLAGS(EditorScene::DecorationFilter)

}

#endif // EDITORSCENE_H
