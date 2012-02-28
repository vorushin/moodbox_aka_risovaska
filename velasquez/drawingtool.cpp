#include "drawingtool.h"

#include <QUndoStack>
#include <QUndoCommand>
#include <QGraphicsSceneMouseEvent>
#include <QKeyEvent>
#include <QMimeData>
#include <QRegExp>
#include <QUrl>
#include <QKeySequence>

#include "drawingelement.h"
#include "editorscene.h"
#include "settingsprovider.h"
#include "undocommands.h"

#include "debug.h"

namespace Velasquez
{

// Comment to hide debug
//#define SHOW_DRAWINGTOOL_DEBUG					true

#if (defined(UDEBUG)) && (defined(SHOW_DRAWINGTOOL_DEBUG))
#define DRAWINGTOOLDEBUG(x)	QDEBUG(x)
#else
#define DRAWINGTOOLDEBUG(x)
#endif

DrawingTool::DrawingTool(QObject *parent)
	: QObject(parent), scene(NULL), undoStack(NULL), settings(NULL), selected(NULL)
{
}

void DrawingTool::setScene(EditorScene *scene)
{
	disconnectSceneSelectionUpdate();

	this->scene = scene;

	if (this->scene != NULL)
	{
		connectSceneSelectionUpdate();

		// We always need to have up-to-date selection
		updateSelection();
	}
}

void DrawingTool::setUndoStack(QUndoStack *undoStack)
{
	this->undoStack = undoStack;
}

bool DrawingTool::hasCursor() const
{
	return false;
}

QCursor DrawingTool::getDefaultCursor() const
{
	return QCursor();
}

bool DrawingTool::useKeySequence(const QKeySequence &sequence)
{
	Q_UNUSED(sequence);

	return false;
}

void DrawingTool::setSettingsProvider(SettingsProvider *settings)
{
	SettingsProvider::connectSettings(this, this->settings, settings);
	
	this->settings = settings;

	updateSelectedSettings();
}

bool DrawingTool::destroyElement(DrawingElement *element)
{
	if (element->type() != getElementType())
		return false;

	onRemoveElement(element);

	return true;
}

bool DrawingTool::addElement(DrawingElement *element)
{
	if (element->type() != getElementType())
		return false;

	if (element->scene() != this->scene)
		this->scene->addItem(element);

	return true;
}
		
void DrawingTool::clearSelection()
{
	if (!hasSelection())
		return;
	
	selected->setSelected(false);
}

void DrawingTool::clearSelectionFocus()
{
	if (!hasSelection())
		return;
	
	selected->clearFocus();
}

bool DrawingTool::hasDeletableElements() const
{
	return !getElementsFromScene().isEmpty();
}

void DrawingTool::deleteSelected()
{
	if (!hasSelection())
		return;
	
	onRemoveElement(selected);
}

void DrawingTool::deleteAll()
{
	QList <DrawingElement *> elements = getElementsFromScene();

	if (elements.isEmpty())
		return;

	if (undoStack)
		undoStack->beginMacro(tr(DELETE_COMMAND_NAME));

	foreach (DrawingElement *element, elements)
		onRemoveElement(element);

	if (undoStack)
		undoStack->endMacro();
}

void DrawingTool::updateElementSettings(DrawingElement *element)
{
	if (this->settings == NULL)
		return;

	QList <qint32> settingsList = element->getSettingsList();

	foreach (qint32 id, settingsList)
		element->setSetting(id, settings->getSetting(id));
}

void DrawingTool::updateSelectedSettings()
{
	if (!hasSelection())
		return;

	updateElementSettings(selected);
}

void DrawingTool::setupNewElement(DrawingElement *element, const QPointF &pos)
{
	updateElementSettings(element);
	
	element->setPos(pos);
	element->setZValue(scene->getNewZOrder());
	
	doCommand(new AddElementCommand(element, this->scene));
}

void DrawingTool::extractElement(DrawingElement *element)
{
	Q_UNUSED(element)
}

QList <DrawingElement *> DrawingTool::getElementsFromScene() const
{
	QList<QGraphicsItem *> sceneItems = scene->items();
	QList<DrawingElement *> ourItems;

	foreach (QGraphicsItem *sceneItem, sceneItems)
	{
		// Filter items by type
		if (sceneItem->type() == getElementType())
		{
			ourItems.append(qgraphicsitem_cast<DrawingElement *>(sceneItem));
		}
	}

	return ourItems;
}
	
QPointF DrawingTool::getCenteredElementPosition(const QPointF &initialPos, DrawingElement *centerElement) const
{
	QPointF pos = (initialPos.isNull()) ? scene->sceneRect().center() : initialPos;
	
	if (centerElement != NULL)
	{
		pos.rx() -= centerElement->boundingRect().width() / 2;
		pos.ry() -= centerElement->boundingRect().height() / 2;
	}
	
	return pos;
}
	
QPointF DrawingTool::getNextElementPosition(const QPointF &initialPos) const
{
	return QPointF(initialPos.x() + NEW_ELEMENT_INDENT, initialPos.y() + NEW_ELEMENT_INDENT);
}
	
void DrawingTool::doCommand(ElementActionCommand *command)
{
	if (undoStack == NULL)
	{
		command->redo();
	
		delete command;
	}
	else
	{
		undoStack->push(command);
	}
}

void DrawingTool::doMacro(const QList <ElementActionCommand *> &commands, const QString &name)
{
	bool runMacro = (undoStack != NULL && commands.count() > 1);
	
	if (runMacro)
		undoStack->beginMacro(name);

	foreach (ElementActionCommand *command, commands)
		doCommand(command);
	
	if (runMacro)
		undoStack->endMacro();
}

bool DrawingTool::isMimeSupported(const QMimeData *data) const
{
	QList<QUrl> urls = data->urls();
	QStringList extensions = getFileExtensions();

	foreach (QUrl url, urls)
	{
		if (isFileSupported(url.toLocalFile(), extensions))
			return true;
	}
	
	return false;
}
	
QStringList DrawingTool::getSupportedFilesFromMime(const QMimeData *data) const
{
	QList<QUrl> urls = data->urls();
	QStringList extensions = getFileExtensions();
	QStringList fileList;
	
	foreach (QUrl url, urls)
	{
		QString localFileName = url.toLocalFile();
		if (isFileSupported(localFileName, extensions))
			fileList << localFileName;
	}
	
	return fileList;
}

bool DrawingTool::isFileSupported(const QString &fileName, const QStringList &extensions)
{
	foreach (QString ext, extensions)
	{
		QRegExp rExt(ext);
		rExt.setPatternSyntax(QRegExp::Wildcard);

		if (rExt.exactMatch(fileName.toLower()))
			return true;
	}

	return false;
}

void DrawingTool::updateSelection()
{
	QList<QGraphicsItem *> sceneSelectedItems = scene->selectedItems();
	QList<QGraphicsItem *> otherSelection;

	DrawingElement *newSelected = NULL;
	DrawingElement *oldSelected = selected;

	foreach (QGraphicsItem *sceneItem, sceneSelectedItems)
	{
		// Filter items by type
		if (sceneItem->type() == getElementType())
		{
			// Got something different? Use it as a selected alternative
			if (sceneItem != selected && newSelected == NULL)				
				newSelected = qgraphicsitem_cast<DrawingElement *>(sceneItem);
			else
				// Old ones (even it is only one) go to otherSelection
				otherSelection.append(sceneItem);
		}
	}
	
	// Got new?
	if (newSelected != NULL)
	{
		// Let's disconnect from scene update to avoid receiving numerous updates
		disconnectSceneSelectionUpdate();

		// Clear the exceeding elements selection
		foreach (QGraphicsItem *item, otherSelection)
			item->setSelected(false);

		connectSceneSelectionUpdate();
		
		// There can be only one
		selected = newSelected;
	}
	else
		if (otherSelection.isEmpty())
			selected = NULL;

	if (oldSelected != selected)
		emit selectionChanged(oldSelected, selected);

	if (hasSelection())
	{
		// Notify about encountered selection
		emit elementSelected(selected);

		DRAWINGTOOLDEBUG("Selected element of type " << getElementType());
	}
	else
	{
		DRAWINGTOOLDEBUG("No selection for type " << getElementType());
	}
}

void DrawingTool::onSettingChanged(qint32 id)
{
	if (!hasSelection())
		return;

	QVariant newValue = settings->getSetting(id);

	if (selected->isSettingReversible(id))
		doCommand(new ChangeElementSettingCommand(selected, id, newValue));
	else
		selected->setSetting(id, newValue);
}

void DrawingTool::onRemoveElement(DrawingElement *element)
{
	doCommand(new DeleteElementCommand(element, scene));
}

void DrawingTool::connectSceneSelectionUpdate()
{
	if (this->scene != NULL)
		connect(scene, SIGNAL(selectionChanged()), this, SLOT(updateSelection()));
}

void DrawingTool::disconnectSceneSelectionUpdate()
{
	if (this->scene != NULL)
	{
		disconnect(scene, SIGNAL(selectionChanged()), this, SLOT(updateSelection()));
	}
}

}