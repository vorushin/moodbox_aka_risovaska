#ifndef COLORSELECTIONCONTROL_H
#define COLORSELECTIONCONTROL_H

#include <QImage>

#include "colorcontrol.h"

class QMouseEvent;

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
	virtual void paintEvent(QPaintEvent *event);
	virtual void resizeEvent(QResizeEvent *event);

	virtual void mousePressEvent(QMouseEvent *event);
	virtual void mouseMoveEvent(QMouseEvent *event);

	virtual QColor getPixelColor(const QPoint &point) const = 0;
	virtual QPointF getColorPosition(const QColor &color) const = 0;

	virtual void createImage() = 0;
	virtual void resetImage();
	
	virtual void updateColorPoint();

protected:	
	QImage image;

	QPointF colorPoint;
};

#endif // COLORSELECTIONCONTROL_H
