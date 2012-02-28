#ifndef COLORWIDGET_H
#define COLORWIDGET_H

#include "colorcontrol.h"

namespace MoodBox
{

#define COLOR_WIDGET_WIDTH			19
#define COLOR_WIDGET_HEIGHT			16

#define COLOR_WIDGET_NORMAL_MASK    ":/MoodBox/Resources/color_button_normal_mask.png"
#define COLOR_WIDGET_HOVER_MASK		":/MoodBox/Resources/color_button_hover_mask.png"
#define COLOR_WIDGET_SELECTED_MASK  ":/MoodBox/Resources/color_button_selected_mask.png"

// Widget to display a single color of palette, can keep index in palette and has an active state
class ColorWidget : public ColorControl
{
	Q_OBJECT

public:
	ColorWidget(QWidget *parent = 0);
	ColorWidget(const QColor &color, int index = -1, QWidget *parent = 0);
	
	void setIndex(int index);
	inline int getIndex() const { return index; };

	void setActive(bool active);
	inline bool getActive() const { return active; };

	void setReadOnly(bool readOnly);
	inline bool getReadOnly() const { return readOnly; };

	virtual QSize sizeHint() const;

	static QImage getHoverMask();
	static QImage getSelectedMask();
	static QImage getNormalMask();

signals:
	void activationRequest(ColorWidget *colorWidget);
	void editRequest(ColorWidget *colorWidget);

protected:
	virtual void paintEvent(QPaintEvent *event);
	virtual void mousePressEvent(QMouseEvent *event);

	virtual void enterEvent(QEvent *event);
	virtual void leaveEvent(QEvent *event);

private:
	int index;
	bool active;
	bool readOnly;
	bool hovered;

};

}

#endif // COLORWIDGET_H
