#include "newcontactlistgripwidget.h"

#include <QImage>
#include <QMouseEvent>
#include <QPainter>

#include "debug.h"

NewContactListGripWidget::NewContactListGripWidget(QWidget *parent)
	: QWidget(parent), isDragged(false)
{
	setMouseTracking(true);
}

NewContactListGripWidget::~NewContactListGripWidget()
{

}

void NewContactListGripWidget::paintEvent(QPaintEvent *)
{
	QImage image(":/MoodBox/Resources/size_grip.png");
	QPainter p(this);
	p.drawImage(QPoint(0, (height() - image.height()) / 2), image);
}

void NewContactListGripWidget::mousePressEvent(QMouseEvent *)
{
	grabMouse();
	isDragged = true;
}

void NewContactListGripWidget::mouseReleaseEvent(QMouseEvent *)
{
	releaseMouse();
	isDragged = false;
}

void NewContactListGripWidget::mouseMoveEvent(QMouseEvent *event)
{
	if (isDragged)
		emit resizeNeeded(event->globalPos().x());
}

void NewContactListGripWidget::enterEvent(QEvent *)
{
	prevCursor = cursor();
	setCursor(Qt::SizeHorCursor);
}

void NewContactListGripWidget::leaveEvent(QEvent *)
{
	setCursor(prevCursor);
}
