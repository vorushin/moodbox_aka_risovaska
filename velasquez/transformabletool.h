#ifndef TRANSFORMABLETOOL_H
#define TRANSFORMABLETOOL_H

#include "drawingtool.h"

namespace Velasquez
{

class TransformableElement;

// Tool for work with transformable drawing elements
class TransformableTool : public DrawingTool
{
	Q_OBJECT

public:
	TransformableTool(QObject *parent);
	
	virtual bool addElement(DrawingElement *element);

protected:
	virtual void setupNewElement(TransformableElement *element, const QPointF &pos);
	virtual void extractElement(DrawingElement *element);

	// Adjust new element size
	virtual void resizeNewElement(TransformableElement *element);

	// Connect signals to the element
	virtual void connectElement(TransformableElement *element);
	// Disconnect signals to the element
	virtual void disconnectElement(TransformableElement *element);

	inline long getSessionNumber() const { return sessionNumber; };
	inline bool isSessionStarted() const { return sessionStarted; };

protected slots:
	void onChangingSession(bool started);

	void onElementMouseEnter(TransformableElement *element);
	void onElementMouseMove(TransformableElement *element);
	void onElementMouseLeave(TransformableElement *element);

	void onElementMoved(TransformableElement *element, const QPointF &oldPos);
	void onResizeRotateElement(TransformableElement *element, qreal diffScale, qreal diffAngle);
	void onReflectElement(TransformableElement *element);

	void onBringFrontElement(TransformableElement *element);
	void onMoveBackwardElement(TransformableElement *element);
	void onMoveForwardElement(TransformableElement *element);
	void onSendBackElement(TransformableElement *element);

private:
	long sessionNumber;
	bool sessionStarted;
};

}

#endif // TRANSFORMABLETOOL_H
