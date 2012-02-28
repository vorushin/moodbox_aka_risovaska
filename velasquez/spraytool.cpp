#include "spraytool.h"

#include "sprayelement.h"

namespace Velasquez
{

SprayTool::SprayTool(QObject *parent)
	: BrushDrawingTool(parent)
{
}

qint32 SprayTool::getElementType() const
{
	return SprayElement::Type;
}

MouseDrawingElement *SprayTool::createNewElement() const
{
	return new SprayElement();
}

}