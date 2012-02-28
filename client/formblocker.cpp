#include "formblocker.h"

#include <QProgressDialog>
#include <QProgressBar>
#include <QApplication>
#include <QTimer>

namespace MoodBox
{

FormBlocker::FormBlocker(QWidget *parentWidget)
	: QObject(parentWidget), blocked(false), progressDialog(NULL)
{
	this->parentWidget = parentWidget;
	
	timer = new QTimer(this);
	connect(timer, SIGNAL(timeout()), this, SLOT(onTimeoutShowProgressBar()));
}

void FormBlocker::addWidget(QWidget *widget)
{
	if (widget != NULL)
		blockingWidgets << widget;
}

void FormBlocker::addWidgets(const QList <QWidget *> &widgets)
{
	blockingWidgets << widgets;
}

void FormBlocker::block(bool showDialog, const QString &message)
{
	if (blocked)
		return;

	foreach (QWidget *widget, blockingWidgets)
		widget->setEnabled(false);

	blocked = true;

	if (showDialog) 
	{
		if (!message.isEmpty())
			progressDialogMessage = message;
		else
			progressDialogMessage = tr(PROGRESS_LABEL_TEXT);

		timer->start(PROGRESS_DIALOG_SHOW_TIMEOUT);
		timer->setSingleShot(true);
	};
}

void FormBlocker::unblock()
{
	if (!blocked)
		return;

	timer->stop();

	foreach (QWidget *widget, blockingWidgets)
		widget->setEnabled(true);
	
	blocked = false;

	stopProgressDialog();
}

QList <QWidget *> FormBlocker::getBlockingWidgets() const
{
	return blockingWidgets;
}

void FormBlocker::setupEndlessProgressDialog(QProgressDialog *progressDialog)
{
	// Title must be set in retranslate() method of parent
	QProgressBar *bar = new QProgressBar(progressDialog);
	bar->setTextVisible(false);

	progressDialog->setBar(bar);
	progressDialog->setMinimum(0);
	progressDialog->setMaximum(0);

	progressDialog->setWindowModality(Qt::ApplicationModal);
	progressDialog->setWindowFlags((progressDialog->windowFlags() | Qt::Tool) & ~Qt::WindowContextHelpButtonHint & ~Qt::WindowSystemMenuHint);
	
	progressDialog->setAutoReset(false);
	progressDialog->setAutoClose(false);
}

void FormBlocker::startProgressDialog(const QString &message)
{
	progressDialog = new QProgressDialog(message, tr(PROGRESS_CANCEL_TEXT), 0, 0, parentWidget);
	setupEndlessProgressDialog(progressDialog);

	connect(progressDialog, SIGNAL(canceled()), this, SLOT(onCancelProgress()));
	progressDialog->show();

	QApplication::setOverrideCursor(Qt::WaitCursor);
}

void FormBlocker::stopProgressDialog()
{
	if (progressDialog == NULL)
		return;

	QApplication::restoreOverrideCursor();

	progressDialog->deleteLater();
	progressDialog = NULL;
}

void FormBlocker::onCancelProgress()
{
	unblock();

	emit progressCancelled();
}


void FormBlocker::onTimeoutShowProgressBar() 
{
	startProgressDialog(progressDialogMessage);
}

}

