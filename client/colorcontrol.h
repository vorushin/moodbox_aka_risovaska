#ifndef COLORCONTROL_H
#define COLORCONTROL_H

#include <QWidget>
#include <QColor>

namespace MoodBox
{

// Base class for controls working with color
class ColorControl : public QWidget
{
	Q_OBJECT

public:
	ColorControl(QWidget *parent = 0);
	ColorControl(const QColor &color, QWidget *parent = 0);
	
	inline QColor getColor() const { return color; };

public slots:
	virtual void setColor(const QColor &color);

protected:
	QColor color;
};

}

#endif // COLORCONTROL_H
