#include "svgtool.h"

#include <QGraphicsSceneMouseEvent>
#include <QGraphicsScene>
#include <QMimeData>
#include <QUrl>

#include "svgelement.h"
#include "vcommon.h"

namespace Velasquez
{

QSizeF SvgTool::defaultSize;

SvgTool::SvgTool(QObject *parent)
	: TransformableTool(parent)
{
}

qint32 SvgTool::getElementType() const
{
	return SvgElement::Type;
}

QStringList SvgTool::getFileExtensions() const
{
	static QStringList ext = QStringList() << SVG_EXT;

	return ext;
}

bool SvgTool::canCreate(QGraphicsSceneMouseEvent *mouseEvent) const
{
	Q_UNUSED(mouseEvent)
	
	return false; 
}

bool SvgTool::canCreate(QKeyEvent *keyEvent) const
{
	Q_UNUSED(keyEvent)
	
	return false; 
}
	
bool SvgTool::canCreate(const QMimeData *data) const
{
	return isMimeSupported(data);
}

void SvgTool::createElement(QGraphicsSceneMouseEvent *mouseEvent)
{
	Q_UNUSED(mouseEvent)
}

void SvgTool::createElement(QKeyEvent *keyEvent)
{
	Q_UNUSED(keyEvent)
}

void SvgTool::createElement(const QMimeData *data, const QPointF &pos)
{
	QPointF addPos;
	
	QStringList files = getSupportedFilesFromMime(data);

	foreach (QString fileName, files)
	{
		SvgElement *element = new SvgElement(fileName);

		// Use default size to fit, if any
		if (!defaultSize.isEmpty())
			element->transformToFit(defaultSize, true /*upsize*/);

		QPointF newPos = getCenteredElementPosition(pos, element) + addPos;
		setupNewElement(element, newPos);

		addPos = getNextElementPosition(addPos);
	}
}

void SvgTool::setDefaultSize(const QSizeF &size)
{
	defaultSize = size;
}

}