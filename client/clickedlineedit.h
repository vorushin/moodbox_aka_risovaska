#ifndef CLICKEDLINEEDIT_H
#define CLICKEDLINEEDIT_H

#include <QLineEdit>

namespace MoodBox
{

class ClickedLineEdit : public QLineEdit
{
	Q_OBJECT

public:
	ClickedLineEdit(QWidget *parent);
	~ClickedLineEdit();

protected:
	virtual void mousePressEvent(QMouseEvent * event);

private:
	
};

}
#endif // CLICKEDLINEEDIT_H
