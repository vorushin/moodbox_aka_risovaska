#include "tvpreviewlabel.h"

#include <QMenu>
#include <QContextMenuEvent>

namespace MoodBox
{

TVPreviewLabel::TVPreviewLabel(QWidget *parent)
	: QLabel(parent), contextMenu(NULL)
{
}

void TVPreviewLabel::setContextMenu(QMenu *menu)
{
	contextMenu = menu;
}

void TVPreviewLabel::enterEvent(QEvent *event)
{
	QLabel::enterEvent(event);

	emit mouseInPreview();
}

void TVPreviewLabel::leaveEvent(QEvent *event)
{
	QLabel::leaveEvent(event);

	emit mouseOutPreview();
}

void TVPreviewLabel::contextMenuEvent(QContextMenuEvent *event)
{
	if (contextMenu != NULL)
	{
		contextMenu->exec(event->globalPos());
	}

	QLabel::contextMenuEvent(event);
}

}