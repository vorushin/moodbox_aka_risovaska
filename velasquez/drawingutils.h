#ifndef DRAWINGUTILS_H
#define DRAWINGUTILS_H

#include <QPointF>
#include <QColor>

namespace Velasquez
{

class DrawingUtils
{
public:
	// Length of line between p1 and p2
	static qreal segmentLength(const QPointF &p1, const QPointF &p2);

	// Normalization of angle to 0..2PI value
	static qreal normalizeAngle(qreal angle);

	// Get random value inside range
	static int rangedRandom(int min, int max);

	// Get random value inside range
	static qreal rangedRandomF(qreal min, qreal max);

	// Get random color deviation in range
	static QColor addRandomColorDeviation(const QColor &baseColor, qreal hueRange, qreal saturationRange, qreal valueRange);

	// Fit value in 0..1 range
	static inline qreal clamp1(qreal value) { return qMax(qMin(value, 1.0), 0.0); };

	// Cycle value in 0..1 range
	static qreal cycle1(qreal value);
};

}

#endif // DRAWINGUTILS_H
