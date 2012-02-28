#include "clipartcategorieslist.h"

#include <QAction>
#include <QDir>
#include <QHBoxLayout>
#include <QToolButton>

#include "apptools.h"
#include "debug.h"
#include "testtools.h"

#ifdef Q_WS_MAC
#include "mactools.h"
#endif


namespace MoodBox
{

ClipartCategoriesList::ClipartCategoriesList(QWidget *parent)
	: QWidget(parent), imageLoader(this, false), startAction(NULL)
{
	connect(&imageLoader, SIGNAL(imagesLoaded(ItemImages, int)), this, SLOT(onClipartItemLoaded(ItemImages, int)));
}

// Most initialization is moved from constructor to allow signals triggered correctly
void ClipartCategoriesList::loadCategories()
{
	TimeMeasure t("ClipartCategoriesList::loadCategories()");

	// Fetch list of clipart subfolders
	QString baseClipartFolder = AppTools::getClipartResourcesFolder();

	QDir clipartFolder(baseClipartFolder);
	
	if (!clipartFolder.exists()) // If clipart folder not exists (e.g. on development computers) we try to find clipart directly in "Clipart"
	{
		baseClipartFolder = CLIPART_DIR;
#ifdef Q_WS_MAC
		baseClipartFolder = "../../" + baseClipartFolder;
#endif
		clipartFolder = QDir(baseClipartFolder);
	}

	QStringList folders = clipartFolder.entryList(QDir::Dirs | QDir::NoDotAndDotDot);

	// Add a button with action for each subfolder
	QHBoxLayout *layout = new QHBoxLayout;
	layout->setContentsMargins(0, 0, 0, 0);
	layout->setSpacing(0);

	for (int i = 0; i < folders.size(); i++)
	{
		QAction *action = new QAction(this);
		action->setToolTip(folders[i]);

		QString categoryFolder = AppTools::addPathSeparator(baseClipartFolder) + folders[i];

		QString iconFile = categoryFolder + CLIPART_ICON_EXTENSION;
		QIcon icon(iconFile);
		action->setIcon(icon);

		QToolButton *button = new QToolButton(this);
		button->setDefaultAction(action);
		button->setCheckable(true);
		connect(button, SIGNAL(triggered(QAction *)), this, SLOT(on_toolbutton_triggered(QAction *)));
		layout->addWidget(button);

		buttons << button;

		clipartCategories[action] = getClipartFolder(categoryFolder, action);
		if (!startAction) startAction = action;
	}

	layout->addStretch();

	this->setLayout(layout);

	if (startAction)
		sendClipartLoadRequest(startAction);
}

ClipartCategory ClipartCategoriesList::getClipartFolder(const QString &folderName, QAction *action)
{
	ClipartCategory result;
	result.folder = folderName;

	QDir dir(folderName);

	Q_ASSERT_X(dir.exists(), "ClipartCategoriesList::getClipartFolder", "Clipart folder is not found");
	if (dir.exists())
	{
		QStringList entries = dir.entryList(QDir::Files | QDir::Readable);
		foreach (QString entry, entries)
		{
			entry = AppTools::addPathSeparator(folderName) + entry;
			ClipartItem item;
			item.action = action;
			item.file = entry;
			clipartFiles << item;
			int id = clipartFiles.size() - 1;
			imageLoader.loadImages(entry, id);
			result.items[entry] = ItemImages();
		}
	}

	return result;
}

void ClipartCategoriesList::sendClipartLoadRequest(QAction *action)
{
	emit clipartLoadRequest(clipartCategories[action].items.values());
}

void ClipartCategoriesList::on_toolbutton_triggered(QAction *action)
{
	// Check off all buttons except clicked one
	foreach (QToolButton *button, buttons)
	{
		button->setChecked(button->defaultAction() == action);
	}

	sendClipartLoadRequest(action);
}

void ClipartCategoriesList::onClipartItemLoaded(ItemImages images, int id)
{
	Q_ASSERT_X(id < clipartFiles.size(), "ClipartCategoriesList::onClipartItemLoaded", "Invalid ID");

	ClipartItem item = clipartFiles[id];

	Q_ASSERT_X(clipartCategories.contains(item.action) && clipartCategories[item.action].items.contains(item.file),
		"ClipartCategoriesList::onClipartItemLoaded", "Invalid data");

	clipartCategories[item.action].items[item.file] = images;

	// Check we fully loaded first clipart page
	if (id > 0)
	{
		QAction *previousAction = clipartFiles[id - 1].action;
		if (item.action != previousAction && previousAction == startAction)
			on_toolbutton_triggered(startAction);
	}
}

}