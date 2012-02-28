#include "pentool.h"

#include "penelement.h"

namespace Velasquez
{

PenTool::PenTool(QObject *parent)
	: BrushDrawingTool(parent)
{
}

qint32 PenTool::getElementType() const
{
	return PenElement::Type;
}

MouseDrawingElement *PenTool::createNewElement() const
{
	return new PenElement();
}

}