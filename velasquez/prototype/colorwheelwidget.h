#ifndef COLORWHEELWIDGET_H
#define COLORWHEELWIDGET_H

#include "colorselectioncontrol.h"

#define COLOR_WHEEL_MARK_RADIUS	2

struct HSV
{
	int h, s, v;
	bool empty;

	inline HSV(int h, int s, int v)
	{
		this->h = h;
		this->s = s;
		this->v = v;

		empty = false;
	};

	inline HSV()
	{
		h = 0;
		v = 0;
		s = 0;

		empty = true;
	};

	inline HSV(const QColor &color)
	{
		h = color.hue();
		s = color.saturation();
		v = color.value();

		empty = false;
	}
};

class ColorWheelWidget : public ColorSelectionControl
{
	Q_OBJECT

public:
	ColorWheelWidget(QWidget *parent = 0);
	ColorWheelWidget(const QColor &color, QWidget *parent = 0);
	~ColorWheelWidget();

public slots:
	virtual void setColor(const QColor &color);

protected:
	virtual void paintEvent(QPaintEvent *event);
	virtual void resizeEvent(QResizeEvent *event);

	QColor getPixelColor(qreal radialPos, qreal angle) const;
	virtual QColor getPixelColor(const QPoint &point) const;
	virtual QPointF getColorPosition(const QColor &color) const;

	virtual void createImage();

private:
	void updateRadius();

	void createBuffer();
	void resetBuffer();
	void updateBufferValue();

	qreal radius;

	HSV *buffer;

};

#endif // COLORWHEELWIDGET_H
