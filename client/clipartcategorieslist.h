#ifndef CLIPARTCATEGORIESLIST_H
#define CLIPARTCATEGORIESLIST_H

#include <QList>
#include <QHash>
#include <QWidget>

#include "clipartimageloader.h"

class QAction;
class QToolButton;

namespace MoodBox
{

#define CLIPART_DIR					"Clipart"
#define CLIPART_ICON_EXTENSION		".svg"

struct ClipartCategory
{
	QString folder;
	QMap<QString,ItemImages> items;
};

struct ClipartItem
{
	QAction *action;
	QString file;
};

// Displays buttons pane to choose clipart category
class ClipartCategoriesList : public QWidget
{
	Q_OBJECT

public:
	ClipartCategoriesList(QWidget *parent);

	void loadCategories();

signals:
	void clipartLoadRequest(const QList<ItemImages> &items);

protected:
	QHash<QAction *, ClipartCategory> clipartCategories;
	QList<QToolButton *> buttons;
	QList<ClipartItem> clipartFiles;
	ClipartImageLoader imageLoader;
	QAction *startAction;

	ClipartCategory getClipartFolder(const QString &folderName, QAction *action);
	void sendClipartLoadRequest(QAction *action);

protected slots:
	void on_toolbutton_triggered(QAction *action);
	void onClipartItemLoaded(ItemImages images, int id);
};

}

#endif // CLIPARTCATEGORIESLIST_H
