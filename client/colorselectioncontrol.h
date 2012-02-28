#ifndef COLORSELECTIONCONTROL_H
#define COLORSELECTIONCONTROL_H

#include <QImage>

#include "colorcontrol.h"

class QMouseEvent;

namespace MoodBox
{

// Base class for color selection controls. Uses QImage-based cache for color presentation
class ColorSelectionControl : public ColorControl
{
	Q_OBJECT

public:
	ColorSelectionControl(QWidget *parent = 0);
	ColorSelectionControl(const QColor &color, QWidget *parent = 0);

public slots:
	virtual void setColor(const QColor &color);

signals:
	void colorSelected(const QColor &color);

protected:
	QImage image;
	QPointF colorPoint;

	int horizontalSpacing, verticalSpacing;
	QRect imageRect;
	QRect maskRect;

	QPoint eventToImage(const QPoint &eventPos) const;

	virtual void paintEvent(QPaintEvent *event);
	virtual void resizeEvent(QResizeEvent *event);

	virtual void mousePressEvent(QMouseEvent *event);
	virtual void mouseMoveEvent(QMouseEvent *event);

	// Spacing operations
	virtual void setSpacing(int horizontal, int vertical);
	virtual void updateImageRect();

	// Get color by position
	virtual QColor getPixelColor(const QPoint &point) const = 0;

	// Get position by color
	virtual QPointF getColorPosition(const QColor &color) const = 0;

	virtual void createImage() = 0;
	virtual void resetImage();
	
	virtual void updateColorPoint();

};

}

#endif // COLORSELECTIONCONTROL_H
