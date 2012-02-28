#include "draggablelabel.h"

#include <QMimeData>
#include <QDrag>
#include <QTemporaryFile>
#include <QUrl>
#include <QMouseEvent>
#include <QDir>
#include <QApplication>

#include "apptools.h"
#include "debug.h"

namespace MoodBox
{

DraggableLabel::DraggableLabel(QWidget *parent, Qt::WindowFlags f)
	: QLabel(parent, f)
{
}

DraggableLabel::DraggableLabel(const QString &text, QWidget *parent, Qt::WindowFlags f)
	: QLabel(text, parent, f)
{
}

void DraggableLabel::mousePressEvent(QMouseEvent *event)
{
	if ( (event->button() == Qt::LeftButton) && (pixmap() != NULL) )
		dragStartPosition = event->pos();

	QLabel::mousePressEvent(event);
}

void DraggableLabel::mouseMoveEvent(QMouseEvent *event)
{
	if (!dragStartPosition.isNull())
	{
		if (!(event->buttons() & Qt::LeftButton))
			return;

		if ((event->pos() - dragStartPosition).manhattanLength() < QApplication::startDragDistance())
			return;
		
		dragPixmap();

		dragStartPosition = QPoint();

		event->accept();
		return;
	}

	QLabel::mouseMoveEvent(event);
}

void DraggableLabel::dragPixmap()
{
	const QPixmap *currentPixmap = pixmap();

	if (currentPixmap == NULL)
		return;
	
	// Make temp file
	QTemporaryFile *tempFile = new QTemporaryFile(AppTools::addPathSeparator(QDir::tempPath()) + DRAG_LABEL_FILENAME_TEMPLATE);
	tempFile->setAutoRemove(false);

	if (!tempFile->open())
	{
		delete tempFile;
		return;
	}
	
	// Arrange data
	QMimeData *data = new QMimeData;
	data->setImageData(currentPixmap->toImage());

	// Save pixmap
	QString tempFileName = tempFile->fileName();
	currentPixmap->save(tempFileName);
	delete tempFile;

	QDEBUG("Dragged file saved to " << tempFileName);

	// Keep its name
	QList <QUrl> urls;
	urls.append(QUrl::fromLocalFile(tempFileName));
	data->setUrls(urls);

	QPixmap preview = currentPixmap->scaled(DRAG_LABEL_PREVIEW_WIDTH, DRAG_LABEL_PREVIEW_HEIGHT, Qt::KeepAspectRatio);

	QPixmap preview2(preview.width(), preview.height());
	preview2.fill(QColor(180, 180, 180));

	preview.setAlphaChannel(preview2);

	// Go drag
	QDrag *drag = new QDrag(this);

	drag->setPixmap(preview);
    drag->setMimeData(data);
	drag->setHotSpot(QPoint(preview.width() / 2, preview.height() / 2));

    // Let's go!
	drag->exec(Qt::MoveAction);
}

}
