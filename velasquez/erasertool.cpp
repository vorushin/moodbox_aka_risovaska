#include "erasertool.h"

#include "eraserelement.h"

namespace Velasquez
{

EraserTool::EraserTool(QObject *parent)
	: BrushDrawingTool(parent)
{
}

qint32 EraserTool::getElementType() const
{
	return EraserElement::Type;
}

MouseDrawingElement *EraserTool::createNewElement() const
{
	return new EraserElement();
}

}