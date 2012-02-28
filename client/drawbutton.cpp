#include "drawbutton.h"

namespace MoodBox
{

DrawButton::DrawButton(QWidget *parent)
	: QPushButton(parent)
{
}

void DrawButton::dragEnterEvent(QDragEnterEvent *event)
{
	if (!isChecked())
		setChecked(true);

	QPushButton::dragEnterEvent(event);
}

}
