#ifndef SIMPLEBRUSHTOOL_H
#define SIMPLEBRUSHTOOL_H

#include "brushdrawingtool.h"

namespace Velasquez
{

// Simple brush tool
class SimpleBrushTool : public BrushDrawingTool
{
	Q_OBJECT

public:
	SimpleBrushTool(QObject *parent);

	virtual qint32 getElementType() const;

protected:
	virtual MouseDrawingElement *createNewElement() const;
	
};
	
}

#endif // SIMPLEBRUSHTOOL_H
