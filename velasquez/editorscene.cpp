#include "editorscene.h"

#include <QPainter>
#include <QEvent>
#include <QGraphicsSceneDragDropEvent>

#include "drawingelement.h"
#include "drawingtool.h"
#include "toolbox.h"
#include "scenecursorlayer.h"
#include "backgroundelement.h"

#include "vcommon.h"
#include "debug.h"

namespace Velasquez
{

EditorScene::EditorScene(QObject *parent)
	: QGraphicsScene(parent), background(NULL), toolBox(NULL), selectedElement(NULL), z(0), mouseInitialized(false)
{
	setSceneRect(QRectF(0, 0, SCENE_WIDTH, SCENE_HEIGHT));
	setItemIndexMethod(QGraphicsScene::NoIndex);
	
	cursorLayerItem = new SceneCursorLayer(this);
	
	addDecorationType(SceneCursorLayer::Type, SceneDecoration);

	addItem(cursorLayerItem);

	connect(this, SIGNAL(selectionChanged()), this, SLOT(onSelectionChanged()));
}

void EditorScene::addItem(QGraphicsItem *item)
{
	QGraphicsScene::addItem(item);

	emit itemAdded(item);
}

void EditorScene::removeItem(QGraphicsItem *item)
{
	QGraphicsScene::removeItem(item);

	emit itemRemoved(item);
}

void EditorScene::clear()
{
	background = NULL;

	clearSelection();
	clearFocus();

	foreach (QGraphicsItem *item, getRootItems())
		delete item;

	emit cleared();
}

void EditorScene::setBackgroundColor(const QColor &color)
{
	if (color == getBackgroundColor())
		return;

	QList <DrawingElement *> elements = getElements();

	foreach (DrawingElement *element, elements)
		element->sceneEvent(BACKGROUND_COLOR_SETTING, color);

	setBackgroundBrush(color);

	emit backgroundColorChanged(color);
}

QColor EditorScene::getBackgroundColor()
{
	return backgroundBrush().color();
}

void EditorScene::updateBackground(BackgroundElement *newBackground)
{
	if (background != NULL)
	{
		background->setColor(newBackground->getColor());
		delete newBackground;
	}
	else
	{	
		background = newBackground;
		addItem(background);
	}	
}

void EditorScene::addElement(DrawingElement *element)
{
	addElementToScene(element);
}

void EditorScene::addElements(QList <DrawingElement *> elements)
{
	foreach (DrawingElement *element, elements)
		addElementToScene(element);
}

QList <DrawingElement *> EditorScene::getElements() const
{
	return getVisibleRootElements();
}

QList <qint32> EditorScene::getElementTypesList() const
{
	QList <qint32> typesList;

	foreach (DrawingElement *element, getElements())
		if (!typesList.contains(element->type()))
			typesList.append(element->type());

	qSort(typesList);

	return typesList;
}

QList <DrawingElement *> EditorScene::extractElements()
{
	QList <DrawingElement *> rootElements = getVisibleRootElements();

	foreach (DrawingElement *element, rootElements)
		removeItem(element);

	return rootElements;
}

QImage EditorScene::renderToImage()
{
	clearFocus();
	clearSelection();

	QImage image(sceneRect().size().toSize(), QImage::Format_ARGB32);
	image.fill(getBackgroundColor().rgba());

	QPainter painter(&image);
	render(&painter);
	painter.end();

	return image;
}

void EditorScene::setToolBox(ToolBox *toolBox)
{
	if (this->toolBox == toolBox)
		return;

	this->toolBox = toolBox;
}

qreal EditorScene::getNewZOrder(const QGraphicsItem *item, ZOrderAction action) const
{
	if (item->scene() == NULL)
		return item->zValue();

	if (action == SendBack || action == BringFront)
	{
		qreal sceneMinZ, sceneMaxZ;

		getMinAndMaxZOrder(sceneMinZ, sceneMaxZ);

		if (action == SendBack)
		{
			return (item->zValue() != sceneMinZ) ? sceneMinZ - ZORDER_STEP : item->zValue();		
		}
		else
		{
			return (item->zValue() != sceneMaxZ) ? sceneMaxZ + ZORDER_STEP : item->zValue();
		}
	}
	else
	{
		qreal itemBackwardZ, itemForwardZ;

		getItemNextZOrder(item, itemBackwardZ, itemForwardZ);

		return (action == MoveBackward) ? itemBackwardZ : itemForwardZ;
	}
}

void EditorScene::addDecorationType(qint32 itemType, DecorationType decorationType)
{
	if (isDecorationType(itemType))
		return;

	decorationTypes.insert(itemType, decorationType);
}

void EditorScene::addDecorationTypes(QList <qint32> itemTypes, DecorationType decorationType)
{
	foreach (qint32 itemType, itemTypes)
	{
		addDecorationType(itemType, decorationType);
	}
}

void EditorScene::removeDecorationType(qint32 itemType)
{
	decorationTypes.remove(itemType);
}

bool EditorScene::isDecorationType(qint32 itemType, DecorationFilter decorationFilter) const
{
	if (!decorationTypes.contains(itemType))
		return false;

	DecorationType decorationType = decorationTypes.value(itemType);

	if (decorationType == SceneDecoration)
		return (decorationFilter & SceneDecorations);
	else
		return (decorationFilter & ItemDecorations);
}

void EditorScene::setCursor(const QCursor &cursor)
{
	cursorLayerItem->setCursor(cursor);
}

void EditorScene::unsetCursor()
{
	cursorLayerItem->unsetCursor();
}

QGraphicsItem *EditorScene::itemAtFiltered(const QPointF &pos, DecorationFilter decorationFilter) const
{
	QList <QGraphicsItem *> itemsAtPos = items(pos);

	foreach(QGraphicsItem *item, itemsAtPos)
	{
		if (!isDecorationType(item->type(), decorationFilter))
			return item;
	}

	return NULL;
}

QStringList EditorScene::getTagNames() const
{
	static QStringList tagNames;

	if (tagNames.isEmpty())
		tagNames << QString(METAINFO_SCENEELEMENTS_TAGNAME);

	return tagNames;
}

QString EditorScene::getTagContent(const QString &tagName) const
{
	if (tagName == METAINFO_SCENEELEMENTS_TAGNAME)
	{
		QString typesString;

		foreach (qint32 type, getElementTypesList())
			typesString += QString("%1, ").arg(QString::number(type));

		if (!typesString.isEmpty())
			typesString.chop(2);

		return typesString;
	}

	return QString();
}

QString EditorScene::getInfoContentAsXml() const
{
	QString content = MetaInfoProvider::getInfoContentAsXml();

	foreach (DrawingElement *element, getElements())
		content += element->getInfoContentAsXml(); 

	return content;
}

bool EditorScene::event(QEvent *event)
{
	// Mouse event must pass through to base class at least once to initialize it
	if (!mouseInitialized)
	{
		if (event->type() == QEvent::GraphicsSceneHoverMove || event->type() == QEvent::GraphicsSceneMouseMove)
			mouseInitialized = true;
	}
	else 
		if (toolBox != NULL)
		{
			ToolBox::EventHandleResult result = toolBox->handleInputEvent(event);

			if (result == ToolBox::Accepted)
			{
				event->accept();
				return true;
			}
			else
				if (result == ToolBox::Ignored)
				{
					event->ignore();
					return true;
				}		
		}

	return QGraphicsScene::event(event);
}

void EditorScene::dragEnterEvent(QGraphicsSceneDragDropEvent *event)
{
	if (toolBox->handleDragEnterEvent(event) == ToolBox::Accepted)
		event->acceptProposedAction();
	else
		event->ignore();
}

void EditorScene::dragMoveEvent(QGraphicsSceneDragDropEvent *event)
{
	if (toolBox->handleDragMoveEvent(event) == ToolBox::Accepted)
		event->acceptProposedAction();
	else
		event->ignore();
}

void EditorScene::dropEvent(QGraphicsSceneDragDropEvent *event)
{
	if (toolBox->handleDragDropEvent(event) == ToolBox::Accepted)
	{
		QPointF p = event->scenePos();
		event->acceptProposedAction();
	}
	else
		event->ignore();
}

QList <QGraphicsItem *> EditorScene::getRootItems() const
{
	QList <QGraphicsItem *> rootItems;

	foreach (QGraphicsItem *item, items())
	{
		// Filter out decorations
		if (item->parentItem() == NULL && !isDecorationType(item->type()))
			rootItems << item;
	}
	
	return rootItems;
}

QList <DrawingElement *> EditorScene::getVisibleRootElements() const
{
	QList <DrawingElement *> rootElements;

	foreach (QGraphicsItem *item, getRootItems())
	{
		const bool hasVisiblePart = item->sceneBoundingRect().intersects(sceneRect());
		const bool obscured = item->isObscured();

		// Check visibility
		if (!hasVisiblePart || obscured)
			continue;

		rootElements << qgraphicsitem_cast<DrawingElement *> (item);
	}

	return rootElements;
}

void EditorScene::addElementToScene(DrawingElement *element)
{
	z = qMax(z, element->zValue() + ZORDER_STEP);

	if (toolBox == NULL)
	{
		addItem(element);
	}
	else
	{
		DrawingTool *tool = toolBox->getTool(element->type());

		if (tool != NULL)
			tool->addElement(element);
	}
}

void EditorScene::getMinAndMaxZOrder(qreal &minZ, qreal &maxZ) const
{
	minZ = maxZ = 0;

	bool initialized = false;

	foreach (QGraphicsItem *item, items())
	{
		if (isDecorationType(item->type()))
			continue;
		else
		{
			if (!initialized)
			{
				maxZ = minZ = item->zValue();
				initialized = true;
			}
			else
			{
				if (minZ > item->zValue())
					minZ = item->zValue();

				if (maxZ < item->zValue())
					maxZ = item->zValue();
			}
		}
	}
}

void EditorScene::getItemNextZOrder(const QGraphicsItem *item, qreal &backward, qreal &forward) const
{
	backward = forward = 0;

	QList<qreal> zOrders;

	// First we collect all z-values of colliding items, including current one
	zOrders.append(item->zValue());

	foreach (QGraphicsItem *collidingItem, collidingItems(item))
		if (isDecorationType(item->type()))
			continue;
		else
			if (!zOrders.contains(collidingItem->zValue()))
				zOrders.append(collidingItem->zValue());

	// Sort them to get lowest and highest
	qSort(zOrders.begin(), zOrders.end());

	// Find where we are now in this list
	int thisPos = zOrders.indexOf(item->zValue());
	
	// Get lowest
	// If we are first (lowest) - nowhere to go
	if (thisPos == 0)
		backward = item->zValue();
	else
		if (thisPos == 1)
		{
			// Just make 1 step less than first
			backward = zOrders[0] - ZORDER_STEP;
		}
		else
		{
			// If we are in the middle - lets fit between neighbours
			qreal z2 = zOrders[thisPos - 2];
			qreal z1 = zOrders[thisPos - 1];
			backward = z1 - qMin((qreal)ZORDER_STEP, z1 - z2) / 2;
		}

	// Get highest - the same as lowest but in different direction
	if (thisPos == zOrders.count() - 1)
		forward = item->zValue();
	else
		if (thisPos == zOrders.count() - 2)
		{
			forward = zOrders[zOrders.count() - 1] + ZORDER_STEP;
		}
		else
		{
			qreal z2 = zOrders[thisPos + 2];
			qreal z1 = zOrders[thisPos + 1];
			forward = z1 + qMin((qreal)ZORDER_STEP, z2 - z1) / 2;
		}
}

void EditorScene::onSelectionChanged()
{
	QList <QGraphicsItem *> selection = selectedItems();

	selectedElement = NULL;

	if (!selection.isEmpty())
	{
		QGraphicsItem *selectedItem = selection.first();

		if (!isDecorationType(selectedItem->type()))
			selectedElement = qgraphicsitem_cast<DrawingElement *>(selectedItem);
	}
}

}