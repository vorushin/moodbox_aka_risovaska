#ifndef PUBLISHDIALOG_H
#define PUBLISHDIALOG_H

#include "ui_publishdialog.h"

#include "publishmessageinfo.h"
#include "publishingway.h"
#include "uitools.h"

namespace MoodBox
{

#define	PUBLISHING_COMPLETED_TEXT	QT_TRANSLATE_NOOP("MoodBox::PublishDialog", "PublishingCompleted")
#define	PUBLISHINGLJ_COMPLETED_TEXT	QT_TRANSLATE_NOOP("MoodBox::PublishDialog", "PublishingCompletedWithLJ")

#define	PUBLISHING_ERROR_TITLE		QT_TRANSLATE_NOOP("MoodBox::PublishDialog", "PublishingErrorTitle")
#define	PUBLISHING_ERROR_TEXT		QT_TRANSLATE_NOOP("MoodBox::PublishDialog", "PublishingErrorText")

#define	POST_LJ_LOGIN_ERROR_TITLE	QT_TRANSLATE_NOOP("MoodBox::PublishDialog", "PostLJLoginErrorTitle")
#define	POST_LJ_LOGIN_ERROR_TEXT	QT_TRANSLATE_NOOP("MoodBox::PublishDialog", "PostLJLoginErrorText%1")

#define	POST_LJ_ERROR_TITLE			QT_TRANSLATE_NOOP("MoodBox::PublishDialog", "PostLJErrorTitle")
#define	POST_LJ_ERROR_TEXT			QT_TRANSLATE_NOOP("MoodBox::PublishDialog", "PostLJErrorText%1")

#define PUBLISHING_METHOD_OPTION	"PublishMethod"
#define PUBLISHING_TYPE_OPTION		"PublishingType"
#define PUBLISHING_TO_BLOG_OPTION   "PublishingToBlog"

#define PUBLISHING_TYPE_LIVEJOURNAL_TEXT	QT_TRANSLATE_NOOP("MoodBox::PublishDialog", "PublishingBlogLiveJournal")
#define PUBLISHING_TYPE_LIVEINTERNET_TEXT	QT_TRANSLATE_NOOP("MoodBox::PublishDialog", "PublishingBlogLiveInternet")
#define PUBLISHING_TYPE_DAIRY_TEXT			QT_TRANSLATE_NOOP("MoodBox::PublishDialog", "PublishingBlogDairy")

#define LIVEJOURNAL_TYPE_INDEX		0
#define LIVEINTERNET_TYPE_INDEX		1
#define DIARY_TYPE_INDEX			2


class LJPoster;
class LIPoster;
class DiaryPoster;

using namespace Ui;

class PublishDialog : public MoodBoxDialog, public PublishDialogClass
{
	Q_OBJECT

public:
	PublishDialog(QWidget *parent = NULL);

	void setMessages(const QList<PublishMessageInfo> &messages);
	inline void setRecipientId(qint32 id) { publishRecipientId = id; };

	void cleanup();

public slots:
	virtual void accept();
	virtual void reject();

protected:
	QList<PublishMessageInfo> messages;
	qint32 publishRequestId, publishRecipientId;
	bool loginsLoaded;

	LJPoster *ljPoster;
	LIPoster *liPoster;
	DiaryPoster *diaryPoster;

	enum Page { Options = 0, Progress = 1, Result = 2 };
	QList <PublishingWay> urls;

	// Opens page in stacked widget
	void setPage(int pageNum);
	void openOptionsPage();
	void openProgressPage();
	void openResultPage();

	void setPublishingProgress(int value);

	virtual void showEvent(QShowEvent *event);

	void copyToClipboard(QString text);

	void startPublishing();
	void startBlogLoginCheck();
	void postToBlog();

	void restorePublishParameters();
	void savePublishParameters();

protected slots:
	void on_publishingButton_clicked();

	void on_blogCheckBox_stateChanged(int state);
	void on_savePasswordCheck_stateChanged(int state);
	void on_loginCombo_currentIndexChanged(const QString& text);

	void onPublishProgress(qint32 id, qint32 percentDone);
	void onPublishCompleted(qint32 id, qint32 moodstripId, const QList<PublishingWay> &urls);
	void onPublishError(qint32 id);

	void onBlogLoginChecked(bool success, const QString &errorMessage);
	void onBlogPostCompleted(bool success, const QString &info);

	void publishEnabled();

	void loadPublishingParameters(int PublishingTypeIndex);

private slots:
	void on_urlButton_clicked();
	void on_blogUrlButton_clicked();
};

}

#endif // PUBLISHDIALOG_H
