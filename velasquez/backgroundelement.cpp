#include "backgroundelement.h"

#include "editorscene.h"
#include "vcommon.h"

namespace Velasquez
{

BackgroundElement::BackgroundElement(QGraphicsItem *parent)
	: DrawingElement(parent)
{
	currentColor = DEFAULT_BACKGROUND_COLOR;
}

void BackgroundElement::setSetting(qint32 id, const QVariant &value)
{
	if (id == BACKGROUND_COLOR_SETTING)
	{
		if (!value.canConvert<QColor>())
				return;

		setColor(value.value<QColor>());
	}
}

QVariant BackgroundElement::getSetting(qint32 id) const
{
	if (id == BACKGROUND_COLOR_SETTING)
		return currentColor;
	else
		return QVariant();
}

QList <qint32> BackgroundElement::getSettingsList() const
{
	QList <qint32> settings;

	settings << BACKGROUND_COLOR_SETTING;

	return settings;
}

void BackgroundElement::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
	Q_UNUSED(painter)
	Q_UNUSED(option)
	Q_UNUSED(widget)
}

QRectF BackgroundElement::boundingRect() const
{
	static QRectF rect(0, 0, 1, 1);

	return rect;
}
	
void BackgroundElement::setColor(const QColor &color)
{
	if (currentColor == color)
		return;

	currentColor = color;
	setSceneColor(scene());
}

QVariant BackgroundElement::itemChange(GraphicsItemChange change, const QVariant &value)
{
	if (change == ItemSceneHasChanged)
	{
		QGraphicsScene *scene = value.value<QGraphicsScene *>();
		setSceneColor(scene);
	}

	return DrawingElement::itemChange(change, value);
}

qint32 BackgroundElement::getType() const
{
	return Type;
}

void BackgroundElement::setSceneColor(QGraphicsScene *scene)
{
	if (scene != NULL)
	{
		EditorScene *editorScene = qobject_cast<EditorScene *> (scene);

		if (editorScene)
			editorScene->setBackgroundColor(currentColor);
		else
			scene->setBackgroundBrush(currentColor);
	}

}

}