#ifndef ELIDEDLABEL_H
#define ELIDEDLABEL_H

#include <QLabel>

namespace MoodBox
{

// Draws QLabel with elided content (w/o changing preferred size)
class ElidedLabel : public QLabel
{
	Q_OBJECT

public:
	ElidedLabel(QWidget *parent = 0);

protected:
	virtual void paintEvent(QPaintEvent *event);
};

}

#endif // ELIDEDLABEL_H
