#include "changepassworddialog.h"

#include <QMessageBox>

#include "serverproxysingleton.h"
#include "peopleinfomanager.h"

#include "verifiers.h"
#include "uitools.h"
#include "logondataprovider.h"
#include "testtools.h"
#include "international.h"

namespace MoodBox
{

// RemoveContactRequest class
ChangePasswordRequest::ChangePasswordRequest(ChangePasswordDialog *parent, const QString &newPassword, const QString &oldPassword)
	: ServerRequest()
{
	connect(this, SIGNAL(changePasswordRequestCompleted(Fault, AccountResultCode::AccountResultCodeEnum)), parent, SLOT(onUpdatePasswordResponse(Fault, AccountResultCode::AccountResultCodeEnum)));
	
	SERVER->updatePassword(CALLBACK(this, onGetChangePasswordRequestResult, AccountResultCode::AccountResultCodeEnum), QVariant(newPassword), newPassword, oldPassword);
}

void ChangePasswordRequest::onGetChangePasswordRequestResult(QVariant state, Fault fault, AccountResultCode::AccountResultCodeEnum result)
{
	if (fault.isNull())
	{
		if (result == AccountResultCode::Ok && INFOMANAGER->isUserOnline())
			LOGONPROVIDER->applyNewPassword(state.toString());
	}

	if (active)
		emit changePasswordRequestCompleted(fault, result);

	deleteLater();
}

// ChangePasswordDialog class
ChangePasswordDialog::ChangePasswordDialog(QWidget *parent)
	: ServerDialog(parent), currentRequest(NULL)
{
	TimeMeasure t("ChangePasswordDialog");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	connect(cancelButton, SIGNAL(clicked()), this, SLOT(reject()));
	connect(closeToolButton, SIGNAL(clicked()), this, SLOT(reject()));

	formBlocker->addWidget(styledFrame);

	WidgetVerifier *passwordsVerifier = new PasswordsVerifier(true, this, currentPasswordLabel);
	formVerifier->addVerifier(passwordsVerifier);

	passwordsVerifier = new PasswordsVerifier(true, this, newPasswordLabel, repeatNewPasswordLabel);
	formVerifier->addVerifier(passwordsVerifier);
}

void ChangePasswordDialog::onUpdatePasswordResponse(Fault fault, AccountResultCode::AccountResultCodeEnum result)
{
	currentRequest = NULL;
	formBlocker->unblock();

	if (!fault.isNull())
	{
		UiTools::handleError(QApplication::activeWindow(), tr(CHANGE_PASSWORD_ERROR_TITLE), fault);
		return;
	}

	QString error;
	switch (result)
	{
		case AccountResultCode::Ok:
			accept();
			break;
		case AccountResultCode::InvalidPassword:
			error = tr(CHANGE_PASSWORD_INVALID_PASSWORD);
			break;
		case AccountResultCode::AccountNotFound:
			error = tr(CHANGE_PASSWORD_ACCOUNT_NOT_FOUND);
			break;
		default:
			error = tr(CHANGE_PASSWORD_UNKNOWN_ERROR);
			break;
	};

	if(!error.isEmpty())
	{
		QMessageBox::warning(QApplication::activeWindow(), tr(CHANGE_PASSWORD_ERROR_TITLE), error);
	}
}

void ChangePasswordDialog::onRequestCancelled()
{
	if (currentRequest != NULL)
		currentRequest->detach();

	formBlocker->unblock();
}

void ChangePasswordDialog::on_okButton_clicked()
{
	if (!formVerifier->verifyAndHighlight(true))
		return;

	formBlocker->block(true);

	currentRequest = new ChangePasswordRequest(this, newPasswordEdit->text(), currentPasswordEdit->text());
}

}
