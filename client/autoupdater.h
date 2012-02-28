#ifndef AUTOUPDATER_H
#define AUTOUPDATER_H

#include <QHttp>
#include <QNetworkProxy>
#include <QAuthenticator>
#include <QBuffer>
#include <QTimer>
#include <QDateTime>
#include <QWidget>
#include <QProgressDialog>

#include "version.h"
#include "language.h"
#include "newversionavailabledialog.h"

namespace MoodBox
{

#ifndef RUSSIAN_VERSION
  #define LATEST_VERSION_URL			"http://moodbox.com/latest-version.txt"
  #define LATEST_VERSION_INFO_URL		"http://moodbox.com/latest-version-info_en.html"
  #define DOWNLOAD_URL					"http://moodbox.com/download"
  #define NEWEST_LIGHT_INSTALLER        "MoodboxSetupSilent.exe"
  #define NEWEST_LIGHT_INSTALLER_URL	"http://static.moodbox.com/"NEWEST_LIGHT_INSTALLER
#else
  #define LATEST_VERSION_URL			"http://risovaska.ru/latest-version.txt"
  #define LATEST_VERSION_INFO_URL		"http://risovaska.ru/latest-version-info_ru.html"
  #define DOWNLOAD_URL					"http://risovaska.ru/download"
  #define NEWEST_LIGHT_INSTALLER        "RisovaskaSetupSilent.exe"
  #define NEWEST_LIGHT_INSTALLER_URL	"http://static.risovaska.ru/"NEWEST_LIGHT_INSTALLER
#endif


#define AUTOUPDATE_RETRY_SHORTLY_DELAY	1800000 // 30 minutes
#define AUTOUPDATE_RETRY_LATER_DELAY	72000000 // 20 hours

#define AUTOUPDATE_ERROR_TITLE				QT_TRANSLATE_NOOP("MoodBox::AutoUpdater", "AutoUpdateErrorTitle")
#define AUTOUPDATE_ERROR_TEXT				QT_TRANSLATE_NOOP("MoodBox::AutoUpdater", "AutoUpdateErrorText%1")
#define AUTOUPDATE_CANNOT_OPEN_LINK_TEXT	QT_TRANSLATE_NOOP("MoodBox::AutoUpdater", "AutoUpdateErrorCannotOpenLink")
#define AUTOUPDATE_INCORRECT_RESPONSE_TEXT	QT_TRANSLATE_NOOP("MoodBox::AutoUpdater", "AutoUpdateErrorIncorrectResponse")

#define AUTOUPDATE_CHECK_TITLE				QT_TRANSLATE_NOOP("MoodBox::AutoUpdater", "AutoUpdateCheckingTitle")
#define AUTOUPDATE_CHECK_LABEL				QT_TRANSLATE_NOOP("MoodBox::AutoUpdater", "AutoUpdateCheckingText")
#define AUTOUPDATE_NOUPDATE_TEXT			QT_TRANSLATE_NOOP("MoodBox::AutoUpdater", "NoUpdateFound")

#define AUTOUPDATE_GROUP								"AutoUpdate"
#define AUTOUPDATE_ENABLED_OPTION						"Enabled"
#define AUTOUPDATE_LASTUPDATENOTIFICATIONDATE_OPTION	"LastNotificationDate"
#define AUTOUPDATE_VERSIONTOSKIP_OPTION					"VersionToSkip"

class AutoUpdater : public QObject
{
	Q_OBJECT

public:
	AutoUpdater(QWidget *parentWidget);
	~AutoUpdater();

	bool isAutoUpdateEnabled();
	void setAutoUpdateEnabled(bool enabled);

	void startCheckingPaused();
	void reinitSettings();

	virtual void checkForUpdateInteractive(QWidget *dialogWidget);
signals:
	void downloadNewVersion();

private slots:	
	void start();
	void onRequestFinished(int id, bool error);
	void onProxyAuthenticationRequired(const QNetworkProxy &proxy, QAuthenticator *authenticator);

	void onInteractiveDialog();
	void onCanceled();
	void onDialogFinished(int result);

private:
	bool isCheckingForUpdate;
	bool isInteractive;
	QString errorString;

	QWidget *parentWidget;
	QWidget *dialogWidget;

	bool autoUpdateEnabled;
	QDateTime lastUpdateNotificationDate;
	Version versionToSkip;

	QProgressDialog *progressDialog;
	NewVersionAvailableDialog *dialog;

	QHttp http;
	QBuffer buffer;
	int requestId;

	bool isRequestingDescription;

	QTimer timer;
	QTimer interactiveDialogTimer;

	Version latestVersion;

	void requestForUpdate();

	void shortRetry();
	void tryLater(int delay = AUTOUPDATE_RETRY_LATER_DELAY);

	void stopNonInteractive();

	void abortRequest();
	void cleanupRequest();

	void onInteractiveError(QString error);
	void onInteractiveNoUpdate();

	void showProgressDialog();
	void hideProgressDialog();

	void showDialog(QString descriptionText);
	void hideDialog();

	void checkFinished();

	QDateTime getLastUpdateNotificationDate();
	void updateLastUpdateNotificationDate(bool preventPersisting = false);
	Version getVersionToSkip();
	void setVersionToSkip(Version version);

	void initSettings();
};

}

#endif // AUTOUPDATER_H
