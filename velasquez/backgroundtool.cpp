#include "backgroundtool.h"

#include <QByteArray>
#include <QDataStream>
#include <QMimeData>

#include "backgroundelement.h"
#include "editorscene.h"
#include "undocommands.h"

#include "settingsprovider.h"
#include "vcommon.h"

namespace Velasquez
{

BackgroundTool::BackgroundTool(QObject *parent)
	: DrawingTool(parent)
{
}

void BackgroundTool::setScene(EditorScene *scene)
{
	EditorScene *oldScene = this->scene;

	DrawingTool::setScene(scene);

	if (this->scene != oldScene)
	{
		if (oldScene != NULL)
			disconnect(oldScene, SIGNAL(cleared()), this, SLOT(onSceneCleared()));

		// Move item to the new scene
		if (this->scene != NULL)
		{
			if (!this->scene->hasBackground())
			{
				BackgroundElement *background = new BackgroundElement();
				this->scene->updateBackground(background);
			}

			connect(scene, SIGNAL(cleared()), this, SLOT(onSceneCleared()));
		}
	}
}

qint32 BackgroundTool::getElementType() const
{
	return BackgroundElement::Type;
}

QStringList BackgroundTool::getFileExtensions() const
{
	static QStringList ext = QStringList();

	return ext;
}

bool BackgroundTool::addElement(DrawingElement *element)
{
	if (DrawingTool::addElement(element))
	{
		BackgroundElement *background = qgraphicsitem_cast <BackgroundElement *> (element);
		scene->updateBackground(background);
	
		return true;
	}

	return false;
}

bool BackgroundTool::canCreate(QGraphicsSceneMouseEvent *mouseEvent) const
{
	Q_UNUSED(mouseEvent)
	
	return false; 
}

bool BackgroundTool::canCreate(QKeyEvent *keyEvent) const
{
	Q_UNUSED(keyEvent)
	
	return false; 
}
	
bool BackgroundTool::canCreate(const QMimeData *data) const
{
	return data->hasFormat(BACKGROUND_COLOR_TYPE);
}

void BackgroundTool::createElement(QGraphicsSceneMouseEvent *mouseEvent)
{
	Q_UNUSED(mouseEvent)
}

void BackgroundTool::createElement(QKeyEvent *keyEvent)
{
	Q_UNUSED(keyEvent)
}

void BackgroundTool::createElement(const QMimeData *data, const QPointF &pos)
{
	Q_UNUSED(pos)

	QByteArray colorData = data->data(BACKGROUND_COLOR_TYPE);
    QDataStream dataStream(&colorData, QIODevice::ReadOnly);
	QColor color;
    dataStream >> color;

	if (color.isValid())
		settings->setSetting(BACKGROUND_COLOR_SETTING, color);
}

bool BackgroundTool::hasDeletableElements() const
{
	if (scene == NULL || scene->getBackground() == NULL)
		return false;

	return scene->getBackground()->getColor() != QColor(DEFAULT_BACKGROUND_COLOR);
}

void BackgroundTool::deleteAll()
{
	settings->setSetting(BACKGROUND_COLOR_SETTING, QColor(DEFAULT_BACKGROUND_COLOR));
}

void BackgroundTool::onSettingChanged(qint32 id)
{
	if (id != BACKGROUND_COLOR_SETTING)
	{
		DrawingTool::onSettingChanged(id);
	}
	else
	{
		if (scene != NULL && scene->getBackground() != NULL)
			doCommand(new ChangeElementSettingCommand(scene->getBackground(), BACKGROUND_COLOR_SETTING, settings->getSetting(id)));
	}
}

void BackgroundTool::onSceneCleared()
{
	// When scene is cleared background element is deleted as well
	BackgroundElement *background = new BackgroundElement();
	background->setSetting(BACKGROUND_COLOR_SETTING, QColor(DEFAULT_BACKGROUND_COLOR));

	this->scene->updateBackground(background);
}

}
