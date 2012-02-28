#include "publishdialog.h"

#include <QClipboard>
#include <QDateTime>
#include <QMessageBox>
#include <QUrl>
#include <QDesktopServices>

#include "messagemanager.h"
#include "logondataprovider.h"
#include "blogtools.h"

#include "testtools.h"

namespace MoodBox
{

PublishDialog::PublishDialog(QWidget *parent)
	: MoodBoxDialog(parent), publishRequestId(0), publishRecipientId(-1), loginsLoaded(false)
{
	TimeMeasure t("PublishDialog");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	UiTools::moveWindowToScreenCenter(this);

	typeCombo->addItem(tr(PUBLISHING_TYPE_LIVEJOURNAL_TEXT));
#ifdef RUSSIAN_VERSION
	typeCombo->addItem(tr(PUBLISHING_TYPE_LIVEINTERNET_TEXT));
	typeCombo->addItem(tr(PUBLISHING_TYPE_DAIRY_TEXT));
#endif

	restorePublishParameters();

	// Control signals
	connect(closeToolButton, SIGNAL(clicked()), this, SLOT(reject()));
	connect(cancelButton, SIGNAL(clicked()), this, SLOT(reject()));
	connect(cancelPublishingButton, SIGNAL(clicked()), this, SLOT(reject()));
	connect(closeButton, SIGNAL(clicked()), this, SLOT(accept()));

	connect(moodstripNameEdit, SIGNAL(textChanged(const QString &)), this, SLOT(publishEnabled()));
	connect(typeCombo, SIGNAL(currentIndexChanged(int)), this, SLOT(loadPublishingParameters(int)));

	// Publishing signals
	connect(MESSAGEMANAGER, SIGNAL(publishing(qint32, qint32)), this, SLOT(onPublishProgress(qint32, qint32)));
	connect(MESSAGEMANAGER, SIGNAL(publishCompleted(qint32, qint32, const QList<PublishingWay> &)),
		this, SLOT(onPublishCompleted(qint32, qint32, const QList<PublishingWay> &)));
	connect(MESSAGEMANAGER, SIGNAL(publishError(qint32)), this, SLOT(onPublishError(qint32)));

	// Update values
	on_blogCheckBox_stateChanged(blogCheckBox->checkState());
	openOptionsPage();
}

void PublishDialog::showEvent(QShowEvent * event)
{
	hiddenCheckBox->setChecked(false);
	moodstripNameEdit->clear();
	moodstripNameEdit->setFocus();

	postText->clear();

	QWidget::showEvent(event);
}

void PublishDialog::setMessages(const QList<PublishMessageInfo> &messages)
{
	this->messages = messages;
}

void PublishDialog::cleanup()
{
	loginCombo->clear();
	passwordEdit->clear();
}

void PublishDialog::accept()
{
	MoodBoxDialog::accept();
	openOptionsPage();
}

void PublishDialog::reject()
{
	if (publishRequestId)
	{
		MESSAGEMANAGER->cancelPublish(publishRequestId);
		publishRequestId = 0;
	}

	MoodBoxDialog::reject();
	openOptionsPage();
}

void PublishDialog::setPage(int pageNum)
{
	stackedWidget->setCurrentIndex(pageNum);
}

void PublishDialog::openOptionsPage()
{
	setPage(Options);
	publishRequestId = 0;
}

void PublishDialog::openProgressPage()
{
	setPage(Progress);
	setPublishingProgress(0);
}

void PublishDialog::openResultPage()
{
	setPage(Result);
	show();
	raise();
	activateWindow();
}

void PublishDialog::setPublishingProgress(int progress)
{
	publishingProgressBar->setValue(progress);
}

void PublishDialog::copyToClipboard(QString text)
{
	QApplication::clipboard()->setText(text);
}

void PublishDialog::startPublishing()
{
	publishRequestId = QDateTime::currentDateTime().toTime_t();

	MESSAGEMANAGER->publishMoodstrip(publishRequestId, moodstripNameEdit->text(), messages, hiddenCheckBox->isChecked(), publishRecipientId);
}

void PublishDialog::startBlogLoginCheck()
{
	switch (typeCombo->currentIndex())
	{
		case LIVEJOURNAL_TYPE_INDEX:
			ljPoster->checkLoginAndPassword(loginCombo->lineEdit()->text(), passwordEdit->text());
			break;

		case LIVEINTERNET_TYPE_INDEX:
			liPoster->checkLoginAndPassword(loginCombo->lineEdit()->text(), passwordEdit->text());
			break;

		case DIARY_TYPE_INDEX:
			diaryPoster->checkLoginAndPassword(loginCombo->lineEdit()->text(), passwordEdit->text());
			break;
	}
}

void PublishDialog::postToBlog()
{
	QString text;

	switch (typeCombo->currentIndex())
	{
		case LIVEJOURNAL_TYPE_INDEX:
		case DIARY_TYPE_INDEX:
			switch (methodCombo->currentIndex())
			{
				case 0:
					text = embedEdit->text();
					break;

				case 1:
					text = embedMoodstripEdit->text();
					break;

				case 2: 
					text = embedMoodstripCutEdit->text();
					break;
			};
			break;

		case LIVEINTERNET_TYPE_INDEX:
			text = embedMoodstripEdit->text();
			break;
	}
	
	switch (typeCombo->currentIndex())
	{
		case LIVEJOURNAL_TYPE_INDEX:
			text = BlogPoster::getEscapedString(postText->toPlainText() + "\n" + text);
			break;

		case DIARY_TYPE_INDEX:
			text = BlogPoster::getHtmlString(postText->toPlainText() + "\n" + text);
			break;

		case LIVEINTERNET_TYPE_INDEX:
			text = postText->toPlainText() + "\n" + text;
			break;
	}

	switch (typeCombo->currentIndex())
	{
		case LIVEJOURNAL_TYPE_INDEX:
			ljPoster->post(loginCombo->lineEdit()->text(), passwordEdit->text(), moodstripNameEdit->text(), text, true);
			break;
		
		case LIVEINTERNET_TYPE_INDEX:
			liPoster->post(loginCombo->lineEdit()->text(), passwordEdit->text(), moodstripNameEdit->text(), text, true);
			break;

		case DIARY_TYPE_INDEX:
			diaryPoster->post(loginCombo->lineEdit()->text(), passwordEdit->text(), moodstripNameEdit->text(), text, true);
			break;
	}
}

void PublishDialog::restorePublishParameters()
{
	QSettings settings;
	settings.beginGroup(SERVICELOGONPROVIDER->getMainSubgroup());
	settings.beginGroup(SERVICELOGONPROVIDER->getSettingsSubgroup());

	int m = settings.value(PUBLISHING_TYPE_OPTION, 1).toInt();
	if (m < 0)
		m = 1;
	else
	if (m >= typeCombo->count())
		m = typeCombo->count() - 1;
	typeCombo->setCurrentIndex(m);

	m = settings.value(PUBLISHING_METHOD_OPTION, 1).toInt();
	if (m < 0)
		m = 1;
	else
	if (m >= methodCombo->count())
		m = methodCombo->count() - 1;
	methodCombo->setCurrentIndex(m);

	blogCheckBox->setChecked(settings.value(PUBLISHING_TO_BLOG_OPTION, false).toBool());

	loadPublishingParameters(typeCombo->currentIndex());

	settings.endGroup();
	settings.endGroup();
}

void PublishDialog::savePublishParameters()
{
	QSettings settings;
	settings.beginGroup(SERVICELOGONPROVIDER->getMainSubgroup());
	settings.beginGroup(SERVICELOGONPROVIDER->getSettingsSubgroup());

	settings.setValue(PUBLISHING_METHOD_OPTION, methodCombo->currentIndex());
	settings.setValue(PUBLISHING_TYPE_OPTION, typeCombo->currentIndex());
	settings.setValue(PUBLISHING_TO_BLOG_OPTION, blogCheckBox->isChecked());

	settings.endGroup();
	settings.endGroup();
}

void PublishDialog::on_publishingButton_clicked()
{
	if (moodstripNameEdit->text().isEmpty())
	{
		moodstripNameEdit->setFocus();
		return;
	}

	if (publishRequestId) 
		return; // Protection against double click

	// Check Blog 
	if (blogCheckBox->isChecked())
	{
		if (loginCombo->lineEdit()->text().isEmpty())
		{
			loginCombo->lineEdit()->setFocus();
			return;
		}

		if (passwordEdit->text().isEmpty())
		{
			passwordEdit->setFocus();
			return;
		}

		SERVICELOGONPROVIDER->setCurrent(loginCombo->lineEdit()->text(), passwordEdit->text());
	}

	savePublishParameters();

	openProgressPage();

	switch (typeCombo->currentIndex())
	{
		case LIVEJOURNAL_TYPE_INDEX:	
			ljPoster = new LJPoster(this);
			connect(ljPoster, SIGNAL(loginChecked(bool, const QString &)), this, SLOT(onBlogLoginChecked(bool, const QString &)));
			connect(ljPoster, SIGNAL(postCompleted(bool, const QString &)), this, SLOT(onBlogPostCompleted(bool, const QString &)));
			break;

		case LIVEINTERNET_TYPE_INDEX:	
			liPoster = new LIPoster(this);
			connect(liPoster, SIGNAL(loginChecked(bool, const QString &)), this, SLOT(onBlogLoginChecked(bool, const QString &)));
			connect(liPoster, SIGNAL(postCompleted(bool, const QString &)), this, SLOT(onBlogPostCompleted(bool, const QString &)));
			break;

		case DIARY_TYPE_INDEX:	
			diaryPoster = new DiaryPoster(this);
			connect(diaryPoster, SIGNAL(loginChecked(bool, const QString &)), this, SLOT(onBlogLoginChecked(bool, const QString &)));
			connect(diaryPoster, SIGNAL(postCompleted(bool, const QString &)), this, SLOT(onBlogPostCompleted(bool, const QString &)));
			break;
	}

	if (blogCheckBox->isChecked())
		startBlogLoginCheck();
	else
		startPublishing();
}

void PublishDialog::on_blogCheckBox_stateChanged(int state)
{
	const bool visible = state != 0;

	ljControlsWidget->setVisible(visible);

	ljUrlLabel->setVisible(visible);
	ljUrlEdit->setVisible(visible);
	blogUrlButton->setVisible(visible);
	ljMoodstripCutLabel->setVisible(visible);
	embedMoodstripCutEdit->setVisible(visible);

	publishedFrame->layout()->activate();
}

void PublishDialog::on_savePasswordCheck_stateChanged(int state)
{
	bool value = (state == Qt::Checked);
	SERVICELOGONPROVIDER->setIsPasswordSavingEnabled(value);
}

void PublishDialog::on_loginCombo_currentIndexChanged(const QString& text)
{
	if (text.isEmpty())
		passwordEdit->clear();
	else
		passwordEdit->setText(SERVICELOGONPROVIDER->getPassword(text));
}

void PublishDialog::onPublishProgress(qint32 id, qint32 percentDone)
{
	if (id != publishRequestId) return;

	Q_ASSERT_X(percentDone >= 0 && percentDone <= 100, "PublishDialog::onPublishProgress", "Invalid percent");

	setPublishingProgress(percentDone);
}

void PublishDialog::onPublishCompleted(qint32 id, qint32 moodstripId, const QList<PublishingWay> &urls)
{
	Q_UNUSED(moodstripId);

	if (id != publishRequestId) return;

	if (urls.isEmpty())
		onPublishError(id);
	else
	{
		Q_ASSERT_X(moodstripId > 0 && !urls.isEmpty(), "PublishDialog::onPublishCompleted", "Empty parameter");

		this->urls = urls;

		foreach(PublishingWay url, urls)
		{
			switch (url.getCode())
			{
				case UrlCode::SimpleUrl:
					urlEdit->setText(url.getUrl());
					urlEdit->setCursorPosition(0);
					break;

				case UrlCode::EmbeddedFlash:
					embedEdit->setText(url.getUrl());
					embedEdit->setCursorPosition(0);
					break;

				case UrlCode::Lj:
					embedMoodstripEdit->setText(url.getUrl());
					embedMoodstripEdit->setCursorPosition(0);
					break;

				case UrlCode::LjCut:
					embedMoodstripCutEdit->setText(url.getUrl());
					embedMoodstripCutEdit->setCursorPosition(0);
					break;
			}
		}

		publishedLabel->setText(tr(PUBLISHING_COMPLETED_TEXT));

		if (blogCheckBox->isChecked())
			postToBlog();
		else
			openResultPage();
	}
}

void PublishDialog::onPublishError(qint32 id)
{
	if (id != publishRequestId) return;

	QMessageBox::warning(this, tr(PUBLISHING_ERROR_TITLE), tr(PUBLISHING_ERROR_TEXT));
	
	openOptionsPage();
}

void PublishDialog::onBlogLoginChecked(bool success, const QString &errorMessage)
{
	if (!success)
	{
		QMessageBox::warning(this, tr(POST_LJ_LOGIN_ERROR_TITLE), tr(POST_LJ_LOGIN_ERROR_TEXT).arg(errorMessage));
		openOptionsPage();
	}
	else
	{
		SERVICELOGONPROVIDER->saveLogonDataIfAllowed();
		startPublishing();
	}
}

void PublishDialog::onBlogPostCompleted(bool success, const QString &info)
{
	if (!success)
	{
		QMessageBox::warning(this, tr(POST_LJ_ERROR_TITLE), tr(POST_LJ_ERROR_TEXT).arg(info));
		ljUrlEdit->clear();
	}
	else
	{
		publishedLabel->setText(tr(PUBLISHINGLJ_COMPLETED_TEXT));
		ljUrlEdit->setText(info);
		ljUrlEdit->setCursorPosition(0);
	}

	openResultPage();
}

void PublishDialog::publishEnabled()
{
}

void PublishDialog::loadPublishingParameters(int PublishingType)
{
	// load logins
	switch (PublishingType)
	{
		case LIVEJOURNAL_TYPE_INDEX:
			SERVICELOGONPROVIDER->setCurrentService(SERVICE_LJ_NAME);
			break;

		case LIVEINTERNET_TYPE_INDEX:
			SERVICELOGONPROVIDER->setCurrentService(SERVICE_LI_NAME);
			break;

		case DIARY_TYPE_INDEX:
			SERVICELOGONPROVIDER->setCurrentService(SERVICE_DIARY_NAME);
			break;
	}
	
	SERVICELOGONPROVIDER->reload();

	savePasswordCheck->setChecked(SERVICELOGONPROVIDER->getIsPasswordSavingEnabled());

	QString oldLogin = loginCombo->lineEdit()->text();
	QString oldPassword = passwordEdit->text();

	loginCombo->clear();
	loginCombo->addItem(QString());
	QStringList logins = SERVICELOGONPROVIDER->getSavedLogins();

	QString login;
	foreach(login, logins)
		loginCombo->addItem(login);

	if (!loginsLoaded)
	{
		login = SERVICELOGONPROVIDER->getLastLoggedOnLogin();

		if (!login.isEmpty()) 
		{
			loginCombo->lineEdit()->setText(login);

			loginCombo->lineEdit()->selectAll();

			passwordEdit->setText(SERVICELOGONPROVIDER->getPassword(login));
		}

		loginsLoaded = true;
	}
	else
	{
		loginCombo->lineEdit()->setText(oldLogin);
		passwordEdit->setText(oldPassword);
	}

	oldPassword.clear();

	// load publishing methods
	methodCombo->clear();

	if (PublishingType == LIVEJOURNAL_TYPE_INDEX || PublishingType == DIARY_TYPE_INDEX) // LiveJournal and Dairy.ru
		methodCombo->addItem(embedLabel->text().replace('\n',' '));
	
	methodCombo->addItem(moodstripLabel->text().replace('\n',' '));
	
	if (PublishingType == LIVEJOURNAL_TYPE_INDEX) // only for LiveJournal
		methodCombo->addItem(ljMoodstripCutLabel->text().replace('\n',' '));
}

void PublishDialog::on_urlButton_clicked()
{
	QUrl url = QUrl(urlEdit->text());
	QDesktopServices::openUrl(url);
}

void PublishDialog::on_blogUrlButton_clicked()
{
	if (ljUrlEdit->text().isEmpty())
		return;

	QUrl url = QUrl(ljUrlEdit->text());
	QDesktopServices::openUrl(url);
}

}