#ifndef DRAWBUTTON_H
#define DRAWBUTTON_H

#include <QPushButton>

namespace MoodBox
{

class DrawButton : public QPushButton
{
	Q_OBJECT

public:
	DrawButton(QWidget *parent);

protected:
	virtual void dragEnterEvent(QDragEnterEvent *event);	
};

}

#endif // DRAWBUTTON_H
