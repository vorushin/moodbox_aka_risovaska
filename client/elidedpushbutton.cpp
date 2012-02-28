#include "elidedpushbutton.h"

namespace MoodBox
{

ElidedPushButton::ElidedPushButton(QWidget *parent)
	: QPushButton(parent)
{
}

void ElidedPushButton::paintEvent(QPaintEvent *event)
{
	QString originalText = text();
	setText(fontMetrics().elidedText(originalText, Qt::ElideRight, width()));

	QPushButton::paintEvent(event);

	setText(originalText);
}

}