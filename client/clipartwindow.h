#ifndef CLIPARTWINDOW_H
#define CLIPARTWINDOW_H

#include <QWidget>

#include "ui_clipartwindow.h"

#include "clipartpage.h"
#include "uitools.h"
#include "palette.h"

class QToolButton;

namespace MoodBox
{

using namespace Ui;

#define CLIPART_SELECT_FOLDER_DIALOG_TITLE	QT_TRANSLATE_NOOP("MoodBox::ClipartWindow", "SelectFolderDialogTitle")
#define CLIPART_LAST_PHOTO_FOLDER_OPTION	"LastPhotoFolder"

#define DEFAULT_CLIPART_TAB					0

// Draws and handles clipart, photo & background tabs
class ClipartWindow : public MovableWidget, public ClipartWindowClass
{
	Q_OBJECT

public:
	ClipartWindow(QWidget *parent = 0);
	bool isShowFirstTime;

public slots:
	void showPage(ClipartPage::ClipartPageEnum page);

protected:
	virtual void resizeEvent(QResizeEvent *event);

protected slots:
	void fillBackgrounds(const Palette &palette);

private:
	QString photoFolder;

	void setPhotoFolder(const QString &folderName);

private slots:
	void on_browseButton_clicked();
};

}

#endif // CLIPARTWINDOW_H
