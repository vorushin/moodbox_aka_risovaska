#ifndef BRUSHSTROKE_H
#define BRUSHSTROKE_H

#include <QPointF>

namespace Velasquez
{

// Class for brush strokes
class BrushStroke : public QPointF
{
public:
	BrushStroke();
	BrushStroke(qreal x, qreal y);
	BrushStroke(const QPointF &pos, bool interpolated = false);
	BrushStroke(const QPointF &pos, qreal speed, qreal angle, bool interpolated = false);

	// Speed
	inline bool hasSpeed() const { return speedSet; };
	inline void setSpeed(qreal speed) { this->speed = speed; speedSet = true; };
	inline qreal getSpeed() const { return this->speed; };
	inline void clearSpeed() { this->speed = 0; speedSet = false; };

	// Angle
	inline bool hasAngle() const { return angleSet; };
	inline void setAngle(qreal angle) { this->angle = angle; angleSet = true; };
	inline qreal getAngle() const { return this->angle; };
	inline void clearAngle() { this->angle = 0; angleSet = false; };

	// Additional infos
	bool rendered;
	bool interpolated;

private:
	qreal speed;
	qreal angle;

	bool speedSet;
	bool angleSet;
};
	
}

#endif // BRUSHSTROKE_H
