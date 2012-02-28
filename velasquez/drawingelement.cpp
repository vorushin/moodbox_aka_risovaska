#include "drawingelement.h"

#include <QPainter>
#include <QColor>
#include <QStyleOptionGraphicsItem>
#include <QMenu>
#include <QGraphicsSceneContextMenuEvent>
#include <QGraphicsScene>

namespace Velasquez
{

DrawingElement::DrawingElement(QGraphicsItem *parent)
	: QGraphicsItem(parent)
{
}

inline int DrawingElement::type() const
{
	return getType();
}

bool DrawingElement::isSettingReversible(qint32 id) const
{
	Q_UNUSED(id)

	return true; 
}

void DrawingElement::sceneEvent(qint32 id, const QVariant &data)
{
	Q_UNUSED(id)
	Q_UNUSED(data)
}

void DrawingElement::paintBoundingFrame(QPainter *painter)
{
	painter->save();
	
	painter->setRenderHint(QPainter::Antialiasing, false);
	
	QPen pen(Qt::black);
	pen.setWidth(1);
	pen.setStyle(Qt::DashDotLine);

	painter->setPen(pen);

	painter->drawRect(boundingRect());
	painter->restore();
}

bool DrawingElement::showContextMenu(const QPointF &pos)
{
	QMenu *menu = getContextMenu();

	if (menu == NULL)
		return false;

	menu->exec(pos.toPoint());

	return true;
}

void DrawingElement::contextMenuEvent(QGraphicsSceneContextMenuEvent *event)
{
	if (showContextMenu(event->screenPos()))
		event->accept();
	else
		event->ignore();
}

}