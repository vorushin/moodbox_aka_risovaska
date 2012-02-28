#include "simplebrushtool.h"

#include "simplebrushelement.h"

namespace Velasquez
{

SimpleBrushTool::SimpleBrushTool(QObject *parent)
	: BrushDrawingTool(parent)
{
}

qint32 SimpleBrushTool::getElementType() const
{
	return SimpleBrushElement::Type;
}

MouseDrawingElement *SimpleBrushTool::createNewElement() const
{
	return new SimpleBrushElement();
}

}
