#ifndef SPRAYTOOL_H
#define SPRAYTOOL_H

#include "brushdrawingtool.h"

namespace Velasquez
{

class SprayTool : public BrushDrawingTool
{
	Q_OBJECT

public:
	SprayTool(QObject *parent);

	virtual qint32 getElementType() const;

protected:
	virtual MouseDrawingElement *createNewElement() const;
	
};

}

#endif // SPRAYTOOL_H
