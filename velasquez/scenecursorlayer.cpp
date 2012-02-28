#include "scenecursorlayer.h"

#include <QGraphicsScene>

namespace Velasquez
{

SceneCursorLayer::SceneCursorLayer(QGraphicsScene *scene)
	: QObject(NULL), QGraphicsItem(NULL)
{
	rect = scene->sceneRect();
	
	setZValue(CURSOR_LAYER_ZVALUE);

	connect(scene, SIGNAL(sceneRectChanged(const QRectF &)), this, SLOT(onSceneRectChanged(const QRectF &)));
}

void SceneCursorLayer::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget)
{
	Q_UNUSED(painter)
	Q_UNUSED(option)
	Q_UNUSED(widget)
}

QRectF SceneCursorLayer::boundingRect() const
{
	return rect;
}

void SceneCursorLayer::onSceneRectChanged(const QRectF &rect)
{
	this->rect = rect;

	prepareGeometryChange();
	update();
}

}
