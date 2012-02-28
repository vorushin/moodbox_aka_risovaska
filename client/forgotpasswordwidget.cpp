#include "forgotpasswordwidget.h"


#include <QMessageBox>

#include "serverproxysingleton.h"
#include "moodboxcustomserver.h"
#include "international.h"
#include "uitools.h"
#include "formblocker.h"
#include "testtools.h"

namespace MoodBox 
{
// ForgotPasswordRequest class
ForgotPasswordRequest::ForgotPasswordRequest(ForgotPasswordWidget *parent, const QString &login) 
	: ServerRequest()
{
	// Connect parent update
	connect(this, SIGNAL(forgotPasswordRequestCompleted(Fault, AccountResultCode::AccountResultCodeEnum)), parent, SLOT(onResetPasswordResult(Fault, AccountResultCode::AccountResultCodeEnum)));

	SERVER->resetPassword(CALLBACK(this, onForgotPasswordRequestResult, AccountResultCode::AccountResultCodeEnum), QVariant(), login);
}

void ForgotPasswordRequest::onForgotPasswordRequestResult(QVariant state, Fault fault, AccountResultCode::AccountResultCodeEnum result)
{
	Q_UNUSED(state)
	
	if (active)
		emit forgotPasswordRequestCompleted(fault, result);

	deleteLater();
}

// ForgotPasswordDialog class
ForgotPasswordWidget::ForgotPasswordWidget(QWidget *parent)
	: ServerWidget(parent), currentRequest(NULL)
{
	TimeMeasure t("ForgotPasswordWidget");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	connect(nextButton, SIGNAL(clicked()), this, SLOT(onRestorePasswordLinkAction()));
	connect(backButton, SIGNAL(clicked()), this, SLOT(onCancelLinkAction()));
	connect(loginEdit, SIGNAL(returnPressed()), this, SLOT(onRestorePasswordLinkAction()));

	// Validators
	loginVerifier = new LineEditVerifier(true, this, loginLabel, StandardVerifier::Login);
	formVerifier->addVerifier(loginVerifier);
    
#ifdef Q_WS_MAC
    loginEdit->setAttribute(Qt::WA_MacShowFocusRect, false);
#endif
}

void ForgotPasswordWidget::onResetPasswordResult(Fault fault, AccountResultCode::AccountResultCodeEnum result) 
{
	currentRequest = NULL;

	emit waitingStop();

	if (!fault.isNull())
	{
		UiTools::handleError(this, tr(RESTORE_PASSWORD_ERROR_TITLE), fault);
		return;
	}

	if (result != AccountResultCode::Ok) 
	{
		UiTools::handleError(QApplication::activeWindow(), tr(RESTORE_PASSWORD_ERROR_TITLE), PromptHelper::getAccountUpdateErrorName(result));
		return;
	}

	emit finished();
}

void ForgotPasswordWidget::onRequestCancelled()
{
	if (currentRequest != NULL)
		currentRequest->detach();
}

void ForgotPasswordWidget::onRestorePasswordLinkAction()
{
    if (!formVerifier->verifyAndHighlight(false))
		return;
	
	emit waitingStart();
    
	currentRequest = new ForgotPasswordRequest(this, loginEdit->text());
}

void ForgotPasswordWidget::onCancelLinkAction()
{
	emit back();
}

void ForgotPasswordWidget::setLoginField(const QString &login)
{
	loginEdit->setText(login);
}

}
