#include "clickedlineedit.h"

namespace MoodBox
{

ClickedLineEdit::ClickedLineEdit(QWidget *parent)
	: QLineEdit(parent)
{
	
}

ClickedLineEdit::~ClickedLineEdit()
{

}

void ClickedLineEdit::mousePressEvent(QMouseEvent * event)
{
	QLineEdit::mousePressEvent(event);
	selectAll();
}

}