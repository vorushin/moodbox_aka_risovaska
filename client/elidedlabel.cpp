#include "elidedlabel.h"

namespace MoodBox
{

ElidedLabel::ElidedLabel(QWidget *parent)
	: QLabel(parent)
{
}

void ElidedLabel::paintEvent(QPaintEvent *event)
{
	QString originalText = text();
	setText(fontMetrics().elidedText(originalText, Qt::ElideRight, width()));

	QLabel::paintEvent(event);

	setText(originalText);
}

}