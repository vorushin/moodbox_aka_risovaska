#ifndef ELIDEDPUSHBUTTON_H
#define ELIDEDPUSHBUTTON_H

#include <QPushButton>

namespace MoodBox
{

// Draws QPushButton with elided content (w/o changing preferred size)
class ElidedPushButton : public QPushButton
{
	Q_OBJECT

public:
	ElidedPushButton(QWidget *parent = 0);

protected:
	virtual void paintEvent(QPaintEvent *event);
};

}

#endif // ELIDEDPUSHBUTTON_H
