#ifndef COLORVOLUMEWIDGET_H
#define COLORVOLUMEWIDGET_H

#include "colorselectioncontrol.h"

namespace MoodBox
{

#define COLOR_VOLUME_SLIDER_WIDTH	3
#define COLOR_VOLUME_WIDGET_SLIDER_MASK				":/MoodBox/Resources/color_volume_slider_mask.png"
#define COLOR_VOLUME_WIDGET_SLIDER_MASK_MULTIPLY	":/MoodBox/Resources/color_volume_slider_mask2.png"
#define COLOR_VOLUME_WIDGET_MASK					":/MoodBox/Resources/color_volume_mask.png"

// Color volume selection controls
class ColorVolumeWidget : public ColorSelectionControl
{
	Q_OBJECT

public:
	enum Direction {Horizontal, Vertical};
	enum Mode {DarkOnly, DarkToLight};

public:
	ColorVolumeWidget(QWidget *parent = 0);
	ColorVolumeWidget(const QColor &color, QWidget *parent = 0);

	void setDirection(Direction direction);
	inline Direction getDirection() const { return direction; };

	void setMode(Mode mode);
	inline Mode getMode() const { return mode; };

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

static QImage getSliderMaskCopy();
static QImage getSliderMaskMultiply();
static QImage getVolumeMask();

};

}

#endif // COLORVOLUMEWIDGET_H
