#ifndef DRAGGABLELABEL_H
#define DRAGGABLELABEL_H

#include <QLabel>

namespace MoodBox
{

#define DRAG_LABEL_PREVIEW_WIDTH		200		
#define DRAG_LABEL_PREVIEW_HEIGHT		200

#define DRAG_LABEL_FILENAME_TEMPLATE	"mbXXXXXX.png"

// Label that allows to drag image
class DraggableLabel : public QLabel
{
	Q_OBJECT

public:
	DraggableLabel(QWidget *parent = 0, Qt::WindowFlags f = 0);
	DraggableLabel(const QString & text, QWidget *parent = 0, Qt::WindowFlags f = 0) ;

protected:
	virtual void mousePressEvent(QMouseEvent *event);
	virtual void mouseMoveEvent(QMouseEvent *event);

private:
	QPoint dragStartPosition;

	void dragPixmap();
};

}
#endif // DRAGGABLELABEL_H
