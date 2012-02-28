#include "autoupdater.h"

#include <QMessageBox>
#include <QDomDocument>
#include <QUrl>
#include <QSettings>

#include "debug.h"
#include "formblocker.h"
#include "uitools.h"

namespace MoodBox
{

AutoUpdater::AutoUpdater(QWidget *parentWidget): QObject(parentWidget)
{
	progressDialog = NULL;
	dialog = NULL;

	isCheckingForUpdate = false;
	isInteractive = false;

	this->parentWidget = parentWidget;
	dialogWidget = parentWidget;

	timer.setSingleShot(true);
	interactiveDialogTimer.setSingleShot(true);

	connect(&http, SIGNAL(requestFinished(int, bool)), this, SLOT(onRequestFinished(int, bool)));
	connect(&http, SIGNAL(proxyAuthenticationRequired(const QNetworkProxy &, QAuthenticator *)), this, SLOT(onProxyAuthenticationRequired(const QNetworkProxy &, QAuthenticator *)));

	connect(&timer, SIGNAL(timeout()), this, SLOT(start()));
	connect(&interactiveDialogTimer, SIGNAL(timeout()), this, SLOT(onInteractiveDialog()));
}

AutoUpdater::~AutoUpdater()
{
	abortRequest();
	timer.stop();
}

bool AutoUpdater::isAutoUpdateEnabled()
{
	return autoUpdateEnabled;
}
void AutoUpdater::setAutoUpdateEnabled(bool enabled)
{
	QSettings settings;
	settings.beginGroup(AUTOUPDATE_GROUP);	
	settings.setValue(AUTOUPDATE_ENABLED_OPTION, enabled);
	settings.endGroup();

	if(autoUpdateEnabled != enabled)
	{
		autoUpdateEnabled = enabled;
		reinitSettings();
	}
}

void AutoUpdater::startCheckingPaused()
{
	reinitSettings();
}

void AutoUpdater::reinitSettings()
{
	stopNonInteractive();

	initSettings();

	if(isAutoUpdateEnabled() && !isCheckingForUpdate)
		timer.start(3000);
}

void AutoUpdater::checkForUpdateInteractive(QWidget *dialogWidget)
{
	if(isCheckingForUpdate && isInteractive)
		return;

	this->dialogWidget = dialogWidget;

	abortRequest();
	timer.stop();

	isCheckingForUpdate = true;
	isInteractive = true;

	showProgressDialog();

	requestForUpdate();
}

void AutoUpdater::start()
{
	if(isCheckingForUpdate)
		return;

	if(!isAutoUpdateEnabled())
		return;

	// ensure now at least AUTOUPDATE_RETRY_LATER_DELAY msecs since last notification
	QDateTime nextCheckDate = lastUpdateNotificationDate.addMSecs(AUTOUPDATE_RETRY_LATER_DELAY);
	if(!lastUpdateNotificationDate.isNull() && QDateTime::currentDateTime() < nextCheckDate)
	{
		int secsTo = QDateTime::currentDateTime().secsTo(nextCheckDate);
		if(secsTo > 0)
			tryLater(secsTo*1000 + 5000);

		return;
	}

	requestForUpdate();
}

void AutoUpdater::onRequestFinished(int id, bool error)
{
	if (id == requestId) {
		if(error)
		{
			if(isInteractive)
			{
				errorString = http.errorString();
				interactiveDialogTimer.start();
			}
			else
			{
				QDEBUG("AutoUpdater request error (will retry shortly): " << http.errorString());
				shortRetry();
			}

			return;
		}

		int statusCode = http.lastResponse().statusCode();
		if(statusCode != 200) // not OK
		{
			if(isInteractive)
			{
				errorString = tr(AUTOUPDATE_INCORRECT_RESPONSE_TEXT);
				interactiveDialogTimer.start();
			}
			else
			{
				QDEBUG("AutoUpdater request HTTP error (will retry later) " << statusCode);
				tryLater();
			}
	
			return;
		}

		if(!isInteractive && !isAutoUpdateEnabled())
		{
			checkFinished();
			return;
		}

		if(isRequestingDescription)
		{
			QByteArray arr = buffer.buffer();
			QString description = QString::fromUtf8(arr.constData(), arr.length());

			updateLastUpdateNotificationDate();

			hideProgressDialog();
			showDialog(description);
		}
		else
		{
			QString latestVersionString = QString(buffer.buffer());
			latestVersion = Version(latestVersionString);
			QDEBUG("AutoUpdater: our version = " << APP_VERSION.toString() << ", version to skip = " << getVersionToSkip().toString() << ", latest version = " << latestVersion.toString());

			if(latestVersion > getVersionToSkip())
			{
#ifdef _DEBUG
				if(!isInteractive)
					QDEBUG("AutoUpdater: new version found, getting description");
#endif
	
				cleanupRequest();
				isRequestingDescription = true;

				QUrl url = QUrl(LATEST_VERSION_INFO_URL);

				http.setHost(url.host());
				requestId = http.get(url.toString(), &buffer);
			}
			else
			{
				// prevent persisting to recheck if application will be restarted
				updateLastUpdateNotificationDate(true);

				if(isInteractive)
				{
					errorString = QString();
					interactiveDialogTimer.start();
				}
				else
				{
					QDEBUG("AutoUpdater: no new version found (will try later)");
					tryLater();
				}
			}
		}
	}
}

void AutoUpdater::onProxyAuthenticationRequired(const QNetworkProxy &proxy, QAuthenticator *authenticator)
{
	authenticator->setUser(proxy.user());
	authenticator->setPassword(proxy.password());
}

void AutoUpdater::onInteractiveDialog()
{
	if(errorString.isNull())
		onInteractiveNoUpdate();
	else
		onInteractiveError(errorString);
}
void AutoUpdater::onCanceled()
{
	abortRequest();
	checkFinished();

	hideProgressDialog();

	start();
}

void AutoUpdater::onDialogFinished(int result)
{
	hideDialog();

	switch(result)
	{
		case QDialog::Accepted:
			emit downloadNewVersion();
			break;

		case -1:
			setVersionToSkip(latestVersion);
			break;
	}

	tryLater();
}

void AutoUpdater::requestForUpdate()
{
	isCheckingForUpdate = true;

	abortRequest();

	QUrl url = QUrl(LATEST_VERSION_URL);
	http.setHost(url.host());

	QDEBUG("AutoUpdater: checking for new version");
	requestId = http.get(url.toString(), &buffer);
}

void AutoUpdater::shortRetry()
{
	tryLater(AUTOUPDATE_RETRY_SHORTLY_DELAY);
}
void AutoUpdater::tryLater(int delay)
{
	checkFinished();
	timer.start(delay);
}

void AutoUpdater::stopNonInteractive()
{
	if(!(isCheckingForUpdate && isInteractive))
	{
		abortRequest();
		checkFinished();
	}

	timer.stop();
}

void AutoUpdater::abortRequest()
{
	requestId = -1;
	http.abort();

	cleanupRequest();
}
void AutoUpdater::cleanupRequest()
{
	requestId = -1;

	buffer.close();
	buffer.buffer().clear();

	isRequestingDescription = false;
}

void AutoUpdater::onInteractiveError(QString error)
{
	hideProgressDialog();
	QMessageBox::warning(dialogWidget, tr(AUTOUPDATE_ERROR_TITLE), tr(AUTOUPDATE_ERROR_TEXT).arg(error));
	checkFinished();

	start();
}

void AutoUpdater::onInteractiveNoUpdate()
{
	hideProgressDialog();
	QMessageBox::information(dialogWidget, tr(AUTOUPDATE_CHECK_TITLE), tr(AUTOUPDATE_NOUPDATE_TEXT));
	checkFinished();

	start();
}

void AutoUpdater::showProgressDialog()
{
	hideProgressDialog();

	QApplication::setOverrideCursor(Qt::WaitCursor);

	progressDialog = new QProgressDialog(dialogWidget);
	connect(progressDialog, SIGNAL(canceled()), this, SLOT(onCanceled()));
	FormBlocker::setupEndlessProgressDialog(progressDialog);
	progressDialog->setLabelText(tr(AUTOUPDATE_CHECK_LABEL));
	progressDialog->show();
}

void AutoUpdater::hideProgressDialog()
{
	if(progressDialog != NULL)
	{
		progressDialog->hide();
		disconnect(progressDialog, SIGNAL(canceled()), this, SLOT(onCanceled()));
		progressDialog->deleteLater();
		progressDialog = NULL;
	
		QApplication::restoreOverrideCursor();
	}
}

void AutoUpdater::showDialog(QString descriptionText)
{
	hideDialog();

	QWidget *parent = dialogWidget;
	if(!isInteractive)
	{
		 QWidget *active = QApplication::activeWindow();
		 if(active != NULL) // show over active window if there is one
			 parent = active;
	}

	dialog = new NewVersionAvailableDialog(parent);
	connect(dialog, SIGNAL(finished(int)), this, SLOT(onDialogFinished(int)));
	dialog->showDialog(descriptionText, isInteractive, parent == parentWidget);
}

void AutoUpdater::hideDialog()
{
	if(dialog != NULL)
	{
		dialog->hide();
		disconnect(dialog, SIGNAL(finished(int)), this, SLOT(onDialogFinished(int)));

		if(dialog->getIsForcedParentWindowShow())
			dialog->parentWidget()->hide();

		dialog->deleteLater();
		dialog = NULL;
	}
}

void AutoUpdater::checkFinished()
{
	dialogWidget = parentWidget;

	isCheckingForUpdate = false;
	isInteractive = false;
}

QDateTime AutoUpdater::getLastUpdateNotificationDate()
{
	return lastUpdateNotificationDate;
}
void AutoUpdater::updateLastUpdateNotificationDate(bool preventPersisting)
{
	lastUpdateNotificationDate = QDateTime::currentDateTime();

	if(!preventPersisting)
	{
		QSettings settings;

		settings.beginGroup(AUTOUPDATE_GROUP);	
		settings.setValue(AUTOUPDATE_LASTUPDATENOTIFICATIONDATE_OPTION, lastUpdateNotificationDate);
		settings.endGroup();
	}
}
Version AutoUpdater::getVersionToSkip()
{
	if(versionToSkip.isEmpty() || APP_VERSION > versionToSkip)
		return APP_VERSION;

	return versionToSkip;
}

void AutoUpdater::setVersionToSkip(Version version)
{
	versionToSkip = version;

	QSettings settings;

	settings.beginGroup(AUTOUPDATE_GROUP);	
	settings.setValue(AUTOUPDATE_VERSIONTOSKIP_OPTION, versionToSkip.toString());
	settings.endGroup();
}

void AutoUpdater::initSettings()
{
	QSettings settings;

	settings.beginGroup(AUTOUPDATE_GROUP);

	autoUpdateEnabled = settings.value(AUTOUPDATE_ENABLED_OPTION, true).toBool();
	lastUpdateNotificationDate = settings.value(AUTOUPDATE_LASTUPDATENOTIFICATIONDATE_OPTION).toDateTime();
	versionToSkip = Version(settings.value(AUTOUPDATE_VERSIONTOSKIP_OPTION).toString());
}

}