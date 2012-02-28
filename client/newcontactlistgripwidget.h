#ifndef NEWCONTACTLISTGRIPWIDGET_H
#define NEWCONTACTLISTGRIPWIDGET_H

#include <QWidget>
#include <QCursor>

class NewContactListGripWidget : public QWidget	
{
	Q_OBJECT

public:
	NewContactListGripWidget(QWidget *parent = 0);
	~NewContactListGripWidget();

signals:
	void resizeNeeded(int newX);

protected:
	virtual void paintEvent(QPaintEvent *);
	void mousePressEvent(QMouseEvent *);
	void mouseReleaseEvent(QMouseEvent *);
	void mouseMoveEvent(QMouseEvent *event);
	
	void enterEvent(QEvent *);
	void leaveEvent(QEvent *);

private:
	QCursor prevCursor;
	bool isDragged;
};

#endif // NEWCONTACTLISTGRIPWIDGET_H
