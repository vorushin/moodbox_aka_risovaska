#ifndef COLORWHEELWIDGET_H
#define COLORWHEELWIDGET_H

#include "colorselectioncontrol.h"

namespace MoodBox
{

#define COLOR_WHEEL_MARK_INNER_RADIUS	4
#define COLOR_WHEEL_MARK_OUTER_RADIUS	5

#define COLOR_WHEEL_WIDGET_MASK (":/MoodBox/Resources/color_wheel_mask.png")

struct HSV;

// Color wheel control
class ColorWheelWidget : public ColorSelectionControl
{
	Q_OBJECT

public:
	ColorWheelWidget(QWidget *parent = 0);
	ColorWheelWidget(const QColor &color, QWidget *parent = 0);

	virtual ~ColorWheelWidget();

public slots:
	virtual void setColor(const QColor &color);

protected:
	virtual void paintEvent(QPaintEvent *event);
	virtual void resizeEvent(QResizeEvent *event);

	virtual QColor getPixelColor(qreal radialPos, qreal angle) const;
	virtual QColor getPixelColor(const QPoint &point) const;
	virtual QPointF getColorPosition(const QColor &color) const;

	virtual void resetImage();
	virtual void createImage();

private:
	qreal radius;

	HSV *buffer;

	void createBuffer();
	void resetBuffer();
	void updateBufferValue();

static QImage getColorWheelMask();
};

}

#endif // COLORWHEELWIDGET_H
