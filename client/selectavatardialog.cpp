#include "selectavatardialog.h"

#include <QImageReader>
#include <QFileDialog>

#include "apptools.h"
#include "peopleinfomanager.h"
#include "common.h"
#include "debug.h"
#include "testtools.h"

namespace MoodBox
{

SelectAvatarDialog::SelectAvatarDialog(QWidget *parent)
	: MoodBoxDialog(parent), lastRow(1)
{
	TimeMeasure t("SelectAvatarDialog");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	UiTools::moveWindowToScreenCenter(this);

	connect(cancelButton, SIGNAL(clicked()), this, SLOT(reject()));
	connect(closeToolButton, SIGNAL(clicked()), this, SLOT(reject()));
	connect(avatarListWidget, SIGNAL(currentItemChanged(QListWidgetItem *, QListWidgetItem *)), this, SLOT(onItemChanged(QListWidgetItem *, QListWidgetItem *)));
	connect(avatarListWidget, SIGNAL(itemDoubleClicked(QListWidgetItem *)), this, SLOT(onApplyAvatar()));

	UserAccount currentUser = INFOMANAGER->getUserAccount();

	// Settings
	settings.beginGroup(USER_SPECIFIC_PARAMETERS);
	settings.beginGroup(QString().setNum(currentUser.getId()));
	
	// Setup first default userpic
	QPixmap avatar;
	avatar.load(USERPIC_DEFAULT);
	QListWidgetItem *defaultAvatarItem = addAvatarToListWidget(avatar, QString(), 0);
	avatarListWidget->setCurrentItem(defaultAvatarItem);

	// Compose list of supported image formats
	QStringList filters;
	foreach (QByteArray format, QImageReader::supportedImageFormats())
		filters += "*." + format;
	
	// Load avatars which comes with program
	addAvatarsFromFolder(AppTools::getAvatarsResourcesFolder(), filters);

	// Load avatars from user's avatar folder
	addAvatarsFromFolder(getAvatarsFolder(), filters);
}

QPixmap SelectAvatarDialog::getSelectedAvatar() const
{
	QDEBUG("getSelectedAvatar(): " << fileName);

	return QPixmap(fileName);
}

QListWidgetItem* SelectAvatarDialog::addAvatarToListWidget(QPixmap &avatar, const QString &fileName, const int row)
{
	PeopleInfoManager::resizePicture(avatar);

	QSize iconSize(USERPIC_MAX_HEIGHT, USERPIC_MAX_WIDTH);

	QListWidgetItem *newItem = new QListWidgetItem;
	newItem->setSizeHint(iconSize);
	newItem->setIcon(avatar);
	newItem->setData(Qt::UserRole, QVariant::fromValue(fileName));
	
	if (row != -1)
		avatarListWidget->insertItem(row, newItem);
	else
		avatarListWidget->addItem(newItem);

	return newItem;
}

QString SelectAvatarDialog::getAvatarsFolder() const
{
	QString userFolder = INFOMANAGER->getUserSettingsFolder();
	return AppTools::addPathSeparator(userFolder) + MY_PICTURES_FOLDER;
}

void SelectAvatarDialog::addAvatarsFromFolder(const QString &folderPath, const QStringList &filters)
{
	dir.setPath(folderPath);
	
	if (!dir.exists())
		return;

	foreach (QString file, dir.entryList(filters, QDir::Files, QDir::Name))
	{
		QPixmap avatar;
		QString currentFileName = dir.path() + PATH_SEPARATOR + file;
		
		if (avatar.load(currentFileName))
		{
			QListWidgetItem *newItem = addAvatarToListWidget(avatar, currentFileName, lastRow);
			
			if (currentFileName == settings.value(LAST_AVATAR_FILE_OPTION, "").toString())
			{
				avatarListWidget->setCurrentItem(newItem);
				fileName = settings.value(LAST_AVATAR_FILE_OPTION, "").toString();
			}

			lastRow++;
		}
	}
}

void SelectAvatarDialog::on_addAvatarButton_clicked()
{
	pictureFolder = settings.value(LAST_AVATAR_FOLDER_OPTION, QDir::homePath()).toString();

	QString name = QFileDialog::getOpenFileName(NULL, tr(SELECT_PICTURES_TITLE), pictureFolder, tr(SELECT_PICTURES_FILTER).arg(SUPPORTED_PICTURE_FORMATS));
	
	if (!name.isNull())
	{
		settings.setValue(LAST_AVATAR_FOLDER_OPTION, QFileInfo(name).absolutePath());
		
		// Create folder if it doesn't exist and copy to the folder selected avatar 
		dir.setPath(getAvatarsFolder());
		if (!dir.exists())
			dir.mkpath(dir.path());
		
		fileName = dir.path() + PATH_SEPARATOR + QFileInfo(name).fileName();
		
		QPixmap avatar;
		if (avatar.load(name))
		{
			PeopleInfoManager::resizePicture(avatar);
			
			// Save in "png" format (for "gif" support), because Qt failed to save avatar if it was "gif"
			fileName += ".png";
			if (avatar.save(fileName, "PNG"))
				addAvatarToListWidget(avatar, fileName);
		}
		else
			UiTools::showDialog(this, tr(OPEN_AVATAR_ERROR_DIALOG_TITLE), tr(OPEN_AVATAR_ERROR_DIALOG_DESCRIPTION), QMessageBox::Ok);
	}
}

void SelectAvatarDialog::on_okButton_clicked()
{
	onApplyAvatar();
	
	accept();
}

void SelectAvatarDialog::onApplyAvatar()
{
	settings.setValue(LAST_AVATAR_FILE_OPTION, fileName);
}

void SelectAvatarDialog::onItemChanged(QListWidgetItem *current, QListWidgetItem *previous)
{
	Q_UNUSED(previous)

	const QVariant f = current->data(Qt::UserRole);
	fileName = f.value<QString>();
	QDEBUG(fileName.toAscii().data());
}

}
