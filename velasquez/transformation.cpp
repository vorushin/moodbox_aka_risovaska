#include "transformation.h"

#include <QGraphicsItem>
#include <math.h>

#include "vcommon.h"
#include "drawingutils.h"

#include "debug.h"

namespace Velasquez
{

Transformation::Transformation(qreal scale, qreal angle, bool reflect)
{
	if (scale > 0)
		this->scale = scale;

	this->angle = angle;
	this->reflected = reflect;
}

void Transformation::setScale(qreal scale)
{
	if (scale > 0)
		this->scale = scale;
}

void Transformation::rescale(qreal scale)
{
	setScale(getScale() * scale);
}
	
void Transformation::setAngle(qreal angle)
{
	this->angle = DrawingUtils::normalizeAngle(angle);
}

void Transformation::rotate(qreal angle)
{
	setAngle(getAngle() + angle);
}

void Transformation::setReflected(bool reflected)
{
	this->reflected = reflected;
}

void Transformation::reflect()
{
	this->setReflected(!isReflected());
}

QTransform Transformation::getMatrix(qreal cx, qreal cy) const
{
	QTransform matrix;

	if (cx != 0 || cy != 0)
		matrix.translate(cx, cy);

	updateMatrix(matrix);

	if (cx != 0 || cy != 0)
		matrix.translate(-cx, -cy);
 
	return matrix;
}

QTransform Transformation::getMatrix(const QGraphicsItem *centerItem) const
{
	QPointF center = centerItem->boundingRect().center();

	return getMatrix(center.x(), center.y());
}

void Transformation::updateMatrix(QTransform &matrix) const
{
	if (isRotated())
		matrix = matrix.rotate(getAngle());

	if (isScaled())
		matrix = matrix.scale(getScale(), getScale());

	if (isReflected())
		matrix = QTransform(-1, 0, 0, 0, 1, 0, 0, 0, 1) * matrix;
}

Transformation& Transformation::operator += (const Transformation &other)
{
	rescale(other.scale);
	rotate(other.angle);
	setReflected(other.reflected);

	return *this;
}

bool Transformation::operator == (const Transformation &other)
{
	return (scale == other.scale) && (angle == other.angle) && (reflected == other.reflected);
}

}