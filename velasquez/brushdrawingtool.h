#ifndef BRUSHDRAWINGTOOL_H
#define BRUSHDRAWINGTOOL_H

#include "mousedrawingtool.h"

namespace Velasquez
{

class BrushDrawingElement;

class BrushDrawingTool : public MouseDrawingTool
{
	Q_OBJECT

public:
	BrushDrawingTool(QObject *parent);

protected:	
	virtual void startCreating();
	virtual void runCreating(QGraphicsSceneMouseEvent *mouseEvent);
	virtual void finishCreating();

	virtual void timerEvent(QTimerEvent *event);
	virtual void pollAndAddMouse();

	void startPollingTimer(int interval);
	void stopPollingTimer();
	inline bool isPollingRunning() const { return pollingTimerId > 0; };

private:
	int pollingTimerId;
};

}

#endif // BRUSHDRAWINGTOOL_H
