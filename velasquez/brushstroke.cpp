#include "brushstroke.h"

namespace Velasquez
{

BrushStroke::BrushStroke() 
	: QPointF(), rendered(false), interpolated(false), speed(0), angle(0), speedSet(false), angleSet(false)
{
}

BrushStroke::BrushStroke(qreal x, qreal y)
	: QPointF(x, y), rendered(false), interpolated(false), speed(0), angle(0), speedSet(false), angleSet(false)
{
}

BrushStroke::BrushStroke(const QPointF &pos, bool interpolated)
	: QPointF(), rendered(false), speed(0), angle(0), speedSet(false), angleSet(false)
{
	this->interpolated = interpolated;
	
	this->setX(pos.x());
	this->setY(pos.y());
}

BrushStroke::BrushStroke(const QPointF &pos, qreal speed, qreal angle, bool interpolated)
	: QPointF(), rendered(false)
{
	this->interpolated = interpolated;
	
	this->setX(pos.x());
	this->setY(pos.y());

	setSpeed(speed);
	setAngle(angle);
}

}