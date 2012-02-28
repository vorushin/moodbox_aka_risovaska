#ifndef COLORVOLUMEWIDGET_H
#define COLORVOLUMEWIDGET_H

#include "colorselectioncontrol.h"

#define COLOR_VOLUME_SLIDER_WIDTH	4

class ColorVolumeWidget : public ColorSelectionControl
{
	Q_OBJECT

public:
	enum Direction {Horizontal, Vertical};
	enum Mode {DarkOnly, DarkToLight};

	ColorVolumeWidget(QWidget *parent = 0);
	ColorVolumeWidget(const QColor &color, QWidget *parent = 0);

	void setDirection(Direction direction);
	Direction getDirection() const;

	void setMode(Mode mode);
	Mode getMode() const;

public slots:
	virtual void setColor(const QColor &color);

protected:
	virtual void paintEvent(QPaintEvent *event);

	virtual QColor getPixelColor(const QPoint &point) const;
	virtual QPointF getColorPosition(const QColor &color) const;

	virtual void createImage();

private:
	Direction direction;
	Mode mode;
};

#endif // COLORVOLUMEWIDGET_H
