#include "drawingutils.h"

#include "vcommon.h"
#include "mtrandom.h"

#include <QLineF>

namespace Velasquez
{

MTRandom _utilsRandom;

qreal DrawingUtils::segmentLength(const QPointF &p1, const QPointF &p2)
{
	return QLineF(p1, p2).length();
}

qreal DrawingUtils::normalizeAngle(qreal angle)
{
    while (angle < 0)
        angle += 360;
    
	while (angle > 360)
        angle -= 360;

    return angle;
}

int DrawingUtils::rangedRandom(int min, int max)
{
	return _utilsRandom.getNextIntRanged(min, max);
}

qreal DrawingUtils::rangedRandomF(qreal min, qreal max)
{
	return _utilsRandom.getNextRealRanged(min, max);
}

QColor DrawingUtils::addRandomColorDeviation(const QColor &baseColor, qreal hueRange, qreal saturationRange, qreal valueRange)
{
	qreal h = cycle1(baseColor.hueF()			+ _utilsRandom.getNextRealRanged(-hueRange, hueRange));
	qreal s = clamp1(baseColor.saturationF()	+ _utilsRandom.getNextRealRanged(-saturationRange, saturationRange));
	qreal v = clamp1(baseColor.valueF()			+ _utilsRandom.getNextRealRanged(-valueRange, valueRange));

	return QColor::fromHsvF(h, s, v);
}

qreal DrawingUtils::cycle1(qreal value)
{
	qreal cycleValue = value;

	if (value < 0)
		cycleValue = 1 + value;

	if (value > 1)
		cycleValue = value - 1;

	return clamp1(cycleValue);
}

}
