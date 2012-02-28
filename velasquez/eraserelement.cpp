#include "eraserelement.h"

#include <QPainter>

#include "editorscene.h"
#include "vcommon.h"

namespace Velasquez
{

EraserElement::EraserElement(QGraphicsItem *parent)
	: BrushDrawingElement(parent)
{
	currentWidth = DEFAULT_ERASER_WIDTH;
	currentColor = DEFAULT_BACKGROUND_COLOR;
}

void EraserElement::setSetting(qint32 id, const QVariant &value)
{
	if (id == BACKGROUND_COLOR_SETTING)
	{
		if (!value.canConvert<QColor>())
				return;

		QColor color = value.value<QColor>();

		if (color == currentColor)
			return;

		setColor(color);

		if (!isEmpty())
		{
			updateCache();
			update();
		}
	}
	else
		BrushDrawingElement::setSetting(id, value);
}

QVariant EraserElement::getSetting(qint32 id) const
{
	if (id == BACKGROUND_COLOR_SETTING)
		return currentColor;
	else
		return BrushDrawingElement::getSetting(id);
}

QList <qint32> EraserElement::getSettingsList() const
{
	QList <qint32> settings;

	settings << PenWidth << BACKGROUND_COLOR_SETTING;

	return settings;
}

void EraserElement::sceneEvent(qint32 id, const QVariant &data)
{
	if (id == BACKGROUND_COLOR_SETTING)
		setSetting(id, data);
}

QVariant EraserElement::itemChange(GraphicsItemChange change, const QVariant &value)
{
	if (change == ItemSceneHasChanged)
	{
		QGraphicsScene *scene = value.value<QGraphicsScene *>();

		if (scene != NULL)
		{
			EditorScene *editorScene = qobject_cast<EditorScene *>(scene);
			QColor color;

			if (editorScene != NULL)
				color = editorScene->getBackgroundColor();
			else
				color = scene->backgroundBrush().color();

			if (color != currentColor)
			{
				setColor(color);
				
				if (!isEmpty())
					updateCache();
			}
		}
	}

	return BrushDrawingElement::itemChange(change, value);
}

qint32 EraserElement::getType() const
{
	return Type;
}

void EraserElement::setWidth(qreal width)
{
	width *= ERASER_WIDTH_SCALE;

	BrushDrawingElement::setWidth(width);
}

void EraserElement::configureStrokesPainter(QPainter *painter)
{
	BrushDrawingElement::configureStrokesPainter(painter);

	QPen pen(Qt::NoPen);
	painter->setPen(pen);

	QBrush brush(currentColor);
	painter->setBrush(brush);
}

void EraserElement::paintStroke(QPainter *painter, int index)
{
	BrushStroke stroke = strokes.at(index);

	painter->drawEllipse(getStrokeRect(index));
}

QRectF EraserElement::getStrokeRect(int index) const
{
	BrushStroke stroke = strokes.value(index);

	QRectF strokeRect(stroke, QSizeF(currentWidth, currentWidth));

	return strokeRect.translated(-strokeRect.width() / 2, -strokeRect.height() / 2);
}


}