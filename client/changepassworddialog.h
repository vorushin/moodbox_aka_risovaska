#ifndef CHANGEPASSWORDDIALOG_H
#define CHANGEPASSWORDDIALOG_H

#include "ui_changepassworddialog.h"
#include "fault.h"
#include "accountresultcode.h"
#include "servercontrol.h"
#include "serverrequest.h"

namespace MoodBox
{

using namespace Ui;

// Errors
#define CHANGE_PASSWORD_ERROR_TITLE			QT_TRANSLATE_NOOP("MoodBox::ChangePasswordDialog", "ChangePasswordErrorTitle")
#define CHANGE_PASSWORD_INVALID_PASSWORD	QT_TRANSLATE_NOOP("MoodBox::ChangePasswordDialog", "InvalidPasswordError")
#define CHANGE_PASSWORD_ACCOUNT_NOT_FOUND	QT_TRANSLATE_NOOP("MoodBox::ChangePasswordDialog", "AccountNotFoundError")
#define CHANGE_PASSWORD_UNKNOWN_ERROR		QT_TRANSLATE_NOOP("MoodBox::ChangePasswordDialog", "UnknownError")

class ChangePasswordDialog;

// Change password request
class ChangePasswordRequest : public ServerRequest
{
	Q_OBJECT

public:
	ChangePasswordRequest(ChangePasswordDialog *parent, const QString &newPassword, const QString &oldPassword);

signals:
	void changePasswordRequestCompleted(Fault fault, AccountResultCode::AccountResultCodeEnum result);

private slots:
	void onGetChangePasswordRequestResult(QVariant state, Fault fault, AccountResultCode::AccountResultCodeEnum result);
};

// Change password dialog
class ChangePasswordDialog : public ServerDialog, public ChangePasswordDialogClass
{
	Q_OBJECT

public:
	ChangePasswordDialog(QWidget *parent = NULL);

public slots:
	void onUpdatePasswordResponse(Fault fault, AccountResultCode::AccountResultCodeEnum result);

	virtual void onRequestCancelled();

private:
	ChangePasswordRequest *currentRequest;

	void updatePasswordError(AccountResultCode::AccountResultCodeEnum result);

private slots:
	void on_okButton_clicked();
};

}

#endif // CHANGEPASSWORDDIALOG_H
