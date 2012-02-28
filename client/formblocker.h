#ifndef FORMBLOCKER_H
#define FORMBLOCKER_H

#include <QWidget>
#include <QList>

class QProgressDialog;

namespace MoodBox
{

#define PROGRESS_LABEL_TEXT				QT_TRANSLATE_NOOP("MoodBox::FormBlocker", "ProgressLabelText")
#define PROGRESS_CANCEL_TEXT			QT_TRANSLATE_NOOP("MoodBox::FormBlocker", "CancelButton")

// Timeout to wait before showing progress dialog, in msecs
#define PROGRESS_DIALOG_SHOW_TIMEOUT	2000

// Class for blocking list of widgets
class FormBlocker: public QObject
{
	Q_OBJECT

public:
	FormBlocker(QWidget *parentWidget);

	void addWidget(QWidget *widget);
	void addWidgets(const QList <QWidget *> &widgets);

	virtual void block(bool showDialog = false, const QString &message = QString() );
	virtual void unblock();

	inline bool isBlocked() const { return blocked; };

	QList <QWidget *> getBlockingWidgets() const;

	static void setupEndlessProgressDialog(QProgressDialog *progressDialog);

signals:
	void progressCancelled();

protected:
	virtual void startProgressDialog(const QString &message);
	virtual void stopProgressDialog();

protected slots:
	virtual void onCancelProgress();

private:
	QList <QWidget *> blockingWidgets;
	QWidget *parentWidget;
	QTimer *timer;
	QString progressDialogMessage;

	bool blocked;

	QProgressDialog *progressDialog;

private slots:
	void onTimeoutShowProgressBar();
};

}

#endif // FORMBLOCKER_H