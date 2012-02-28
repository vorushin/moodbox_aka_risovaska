#ifndef OILBRUSHTOOL_H
#define OILBRUSHTOOL_H

#include "brushdrawingtool.h"

namespace Velasquez
{

// Oil brush painting tool
class OilBrushTool : public BrushDrawingTool
{
	Q_OBJECT

public:
	OilBrushTool(QObject *parent);

	virtual qint32 getElementType() const;

protected:
	virtual MouseDrawingElement *createNewElement() const;

};

}

#endif // OILBRUSHTOOL_H
