#ifndef COLORCONTROL_H
#define COLORCONTROL_H

#include <QWidget>
#include <QColor>

class ColorControl : public QWidget
{
	Q_OBJECT

public:
	ColorControl(QWidget *parent = 0);
	ColorControl(const QColor &color, QWidget *parent = 0);
	
	QColor getColor() const;

public slots:
	virtual void setColor(const QColor &color);

protected:
	QColor color;
};

qreal getSegmentLength(const QPointF &p1, const QPointF &p2);

#endif // COLORCONTROL_H
