#ifndef TRANSFORMATION_H
#define TRANSFORMATION_H

#include <QTransform>
#include <QMetaType>

class QGraphicsItem;

namespace Velasquez
{

// Class for keep and management of elements transformation info. 
// Provides the way to reverse or change transformations.
class Transformation
{
public:
	Transformation(qreal scale = 1, qreal angle = 0, bool reflect = false);
	
	virtual ~Transformation() {};

	// Scale operations
	// Set scale factor
	virtual void setScale(qreal scale);
	inline qreal getScale() const { return this->scale; };
	// Combine current scale with new one
	void rescale(qreal scale);
	
	inline bool isScaled() const { return this->scale != 1; };

	// Angle operations
	// Set rotation angle
	virtual void setAngle(qreal angle);
	inline qreal getAngle() const { return this->angle; };
	// Combine current angle with new one
	void rotate(qreal angle);

	inline bool isRotated() const { return this->angle != 0; };

	// Reflection operations
	virtual void setReflected(bool reflected);
	// Invert current reflection
	void reflect();

	inline bool isReflected() const { return reflected; };

	// QTransform operations
	// Get transformation matrix centered by cx and cy, if specified
	virtual QTransform getMatrix(qreal cx = 0, qreal cy = 0) const;
	// Get transformation matrix centered by item
	virtual QTransform getMatrix(const QGraphicsItem *centerItem) const;
	// Update existing matrix with current transformations
	virtual void updateMatrix(QTransform &matrix) const;

	Transformation& operator += (const Transformation &other);

	bool operator == (const Transformation &other);

private:
	qreal scale, angle;
	bool reflected;
};

}
Q_DECLARE_METATYPE(Velasquez::Transformation)

#endif // TRANSFORMATION_H
