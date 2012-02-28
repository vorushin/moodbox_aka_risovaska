#ifndef ERASERTOOL_H
#define ERASERTOOL_H

#include "brushdrawingtool.h"

namespace Velasquez
{

// Eraser tool
class EraserTool : public BrushDrawingTool
{
	Q_OBJECT

public:
	EraserTool(QObject *parent);

	virtual qint32 getElementType() const;

protected:
	virtual MouseDrawingElement *createNewElement() const;	

};

}

#endif // ERASERTOOL_H
