#ifndef CLIPARTENTRIESLIST_H
#define CLIPARTENTRIESLIST_H

#include <QByteArray>
#include <QLabel>
#include <QListView>
#include <QStandardItem>

#include "clipartentrieslistmodel.h"
#include "clipartimageloader.h"

namespace MoodBox
{

class ClipartEntriesListModel;

// Number of columns to display in clipart window
#define CLIPART_COLUMNS				5

// Pixels to subtract 
#define LIST_WIDTH_SUBTRACT			3

// Displays list of clipart entries/photos etc.
class ClipartEntriesList : public QListView
{
	Q_OBJECT

public:
	ClipartEntriesList(QWidget *parent = 0);
	
	void setImageEntries(const QList<ItemImages> &images);
	void loadFolder(const QString &folderName);
	
public slots:
	void loadEntries(const QList<ItemImages> &entries);

protected:
	ClipartEntriesListModel *itemModel;

	// Specialized scroller
	QPoint previewStartPos;
	QLabel *previewLabel;

protected:
	virtual void resizeEvent(QResizeEvent *event);
	virtual void startDrag(Qt::DropActions supportedActions);
	virtual void mousePressEvent(QMouseEvent *event);
	virtual void mouseMoveEvent(QMouseEvent *event);	
	virtual void mouseReleaseEvent(QMouseEvent *event);	

	void clearModel();
	
	// Updates preview
	void turnPreview(bool on, QPoint mousePosition = QPoint());

	QPixmap getCurrentPixmap(int pixmapType = CLIPART_ENTRIES_PREVIEW_DATA) const;
	QByteArray getBackgroundData(const QPixmap &pixmap) const;
};

}

#endif
