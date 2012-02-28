#ifndef COLORWIDGET_H
#define COLORWIDGET_H

#include "colorcontrol.h"

class ColorWidget : public ColorControl
{
	Q_OBJECT

public:
	ColorWidget(QWidget *parent = 0);
	ColorWidget(const QColor &color, int index = -1, QWidget *parent = 0);
	
	void setIndex(int index);
	int getIndex() const;

	void setActive(bool active);
	bool getActive() const;

	virtual QSize sizeHint() const;

signals:
	void activationRequest(ColorWidget *colorWidget);
	void editRequest(ColorWidget *colorWidget);

protected:
	virtual void paintEvent(QPaintEvent *event);
	virtual void mousePressEvent(QMouseEvent *event);

private:
	int index;
	bool active;
};

#endif // COLORWIDGET_H
