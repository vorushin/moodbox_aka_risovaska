#include "eyedroppertool.h"

#include <QPixmap>
#include <QGraphicsSceneMouseEvent>

#include "editorscene.h"

namespace Velasquez
{

EyedropperTool::EyedropperTool(QObject *parent)
	: DrawingTool(parent)
{
}

bool EyedropperTool::hasCursor() const
{
	return true;
}

QCursor EyedropperTool::getDefaultCursor() const
{
	static const QCursor DropperCursor(QPixmap(EYEDROPPER_CURSOR), EYEDROPPER_CURSOR_X, EYEDROPPER_CURSOR_Y);

	return DropperCursor;
}

qint32 EyedropperTool::getElementType() const
{
	return Type;
}

QStringList EyedropperTool::getFileExtensions() const
{
	return QStringList();
}

bool EyedropperTool::canCreate(QGraphicsSceneMouseEvent *mouseEvent) const
{
	return mouseEvent->button() == Qt::LeftButton;
}

bool EyedropperTool::canCreate(QKeyEvent *keyEvent) const
{
	Q_UNUSED(keyEvent)

	return false;
}

bool EyedropperTool::canCreate(const QMimeData *data) const
{
	Q_UNUSED(data)

	return false;
}

void EyedropperTool::createElement(QGraphicsSceneMouseEvent *mouseEvent)
{
	QPoint pos = mouseEvent->scenePos().toPoint();

	QImage sceneImage = scene->renderToImage();

	QColor pointColor = QColor(sceneImage.pixel(pos));

	emit colorPicked(pointColor);
}

void EyedropperTool::createElement(QKeyEvent *keyEvent)
{
	Q_UNUSED(keyEvent)
}

void EyedropperTool::createElement(const QMimeData *data, const QPointF &pos)
{
	Q_UNUSED(data)
	Q_UNUSED(pos)
}

}