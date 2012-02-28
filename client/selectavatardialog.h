#ifndef SELECTAVATARDIALOG_H
#define SELECTAVATARDIALOG_H

#include "uitools.h"

#include <QDir>
#include <QSettings>

#include "ui_selectavatardialog.h"

namespace MoodBox
{

using namespace Ui;

// Setting's group for specific user parameters
#define LAST_AVATAR_FOLDER_OPTION	"Last avatar folder"
#define LAST_AVATAR_FILE_OPTION		"Last avatar file"
#define AVATAR_FOLDER				"Avatars"
#define MY_PICTURES_FOLDER			"My pictures"

// Filters and title
#define SELECT_PICTURES_FILTER					QT_TRANSLATE_NOOP("MoodBox::SelectAvatarDialog", "SelectImagesFilter%1")
#define SELECT_PICTURES_TITLE					QT_TRANSLATE_NOOP("MoodBox::SelectAvatarDialog", "SelectImagesTitle")
#define OPEN_AVATAR_ERROR_DIALOG_TITLE			QT_TRANSLATE_NOOP("MoodBox::SelectAvatarDialog", "ErrorOpenAvatarTitle")
#define OPEN_AVATAR_ERROR_DIALOG_DESCRIPTION	QT_TRANSLATE_NOOP("MoodBox::SelectAvatarDialog", "ErrorOpenAvatarDescription")

// Select user avatar from the list and set it on server
class SelectAvatarDialog : public MoodBoxDialog, public SelectAvatarDialogClass
{
	Q_OBJECT

public:
	SelectAvatarDialog(QWidget *parent = NULL);

	inline QString getFileName() const { return fileName; };
	QPixmap getSelectedAvatar() const;

private:
	QDir dir;
	QSettings settings;
	QString pictureFolder;
	QString fileName;

	int lastRow;

	QListWidgetItem* addAvatarToListWidget(QPixmap &avatar, const QString &fileName, const int row = -1);
	
	QString getAvatarsFolder() const;
	void addAvatarsFromFolder(const QString &folderPath, const QStringList &filters);

private slots:
	void on_addAvatarButton_clicked();
	void on_okButton_clicked();

	void onApplyAvatar();
	void onItemChanged(QListWidgetItem *current, QListWidgetItem *previous);
};

}

#endif // SELECTAVATARDIALOG_H
