#ifndef FORGOTPASSWORDWIDGET_H
#define FORGOTPASSWORDWIDGET_H

#include "ui_forgotpasswordwidget.h"
#include "verifiers.h"
#include "fault.h"
#include "accountresultcode.h"
#include "servercontrol.h"
#include "serverrequest.h"

namespace MoodBox 
{

using namespace Ui;

#define RESTORE_PASSWORD_ERROR_TITLE		QT_TRANSLATE_NOOP("MoodBox::ForgotPasswordWidget", "RestorePasswordErrorMessageBoxTitle")

class ForgotPasswordWidget;

// Forgot password request
class ForgotPasswordRequest : public ServerRequest
{
	Q_OBJECT

public:
	ForgotPasswordRequest(ForgotPasswordWidget *parent, const QString &login);
	
signals:
	void forgotPasswordRequestCompleted(Fault fault, AccountResultCode::AccountResultCodeEnum result);

private slots:
	void onForgotPasswordRequestResult(QVariant state, Fault fault, AccountResultCode::AccountResultCodeEnum result);
};

// Forgot password widget
class ForgotPasswordWidget : public ServerWidget, public ForgotPasswordWidgetClass
{
	Q_OBJECT

public:
	ForgotPasswordWidget(QWidget *parent = NULL);

	void clearData() { loginEdit->clear(); };
	void setLoginField(const QString &login);

public slots:
	void onResetPasswordResult(Fault fault, AccountResultCode::AccountResultCodeEnum result);

	virtual void onRequestCancelled();

signals:
	void finished();
	void back();

	void waitingStart();
	void waitingStop();

private:
	WidgetVerifier *loginVerifier;
	ForgotPasswordRequest *currentRequest;

private slots:
	void onRestorePasswordLinkAction();
	void onCancelLinkAction();
};

}

#endif // FORGOTPASSWORDWIDGET_H
