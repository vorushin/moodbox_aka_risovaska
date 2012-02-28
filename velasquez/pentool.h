#ifndef PENTOOL_H
#define PENTOOL_H

#include "brushdrawingtool.h"

namespace Velasquez
{

// Pen painting tool
class PenTool : public BrushDrawingTool
{
	Q_OBJECT

public:
	PenTool(QObject *parent);

	virtual qint32 getElementType() const;

protected:
	virtual MouseDrawingElement *createNewElement() const;

};

}

#endif // PENTOOL_H
