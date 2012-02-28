#ifndef TVPREVIEWLABEL_H
#define TVPREVIEWLABEL_H

#include <QLabel>

class QMenu;

namespace MoodBox
{

// Preview label for TVWidget
class TVPreviewLabel : public QLabel
{
	Q_OBJECT

public:
	TVPreviewLabel(QWidget *parent);

	void setContextMenu(QMenu *menu);

signals:
	void mouseInPreview();
	void mouseOutPreview();

protected:
	QMenu *contextMenu;

	virtual void enterEvent(QEvent *event);
	virtual void leaveEvent(QEvent *event);

	virtual void contextMenuEvent(QContextMenuEvent *event);
};

}

#endif // TVPREVIEWLABEL_H
