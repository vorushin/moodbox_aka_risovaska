#include "oilbrushtool.h"

#include "oilbrushelement.h"

namespace Velasquez
{

OilBrushTool::OilBrushTool(QObject *parent)
	: BrushDrawingTool(parent)
{
}

qint32 OilBrushTool::getElementType() const
{
	return OilBrushElement::Type;
}

MouseDrawingElement *OilBrushTool::createNewElement() const
{
	return new OilBrushElement();
}

}