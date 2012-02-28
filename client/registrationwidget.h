#ifndef REGISTRATIONWIDGET_H
#define REGISTRATIONWIDGET_H

#include <QDialog>
#include <QWidget>

#include "ui_registrationwidget.h"
//TODO ARCH review servercontrol class
#include "servercontrol.h"
#include "accountresultcode.h"
#include "useraccount.h"
#include "fault.h"
#include "serverrequest.h"

namespace MoodBox
{

using namespace Ui;

#define MANDATORY_FIELDS						QT_TRANSLATE_NOOP("MoodBox::RegistrationWidget", "MandatoryFields")

// Errors
#define INVALID_LOGIN_ERROR_MESSAGE				QT_TRANSLATE_NOOP("MoodBox::RegistrationWidget", "InvalidLoginError")
#define LOGIN_IS_NOT_AVAILABLE_ERROR_MESSAGE	QT_TRANSLATE_NOOP("MoodBox::RegistrationWidget", "LoginIsNotAvailableError")

#define REGISTRATION_ERROR_TITLE				QT_TRANSLATE_NOOP("MoodBox::RegistrationWidget", "RegistrationErrorTitle")
#define REGISTRATION_ERROR						QT_TRANSLATE_NOOP("MoodBox::RegistrationWidget", "RegistrationError")

// Urls
#ifndef RUSSIAN_VERSION
  #define TERM_OF_USE_URL							"http://moodbox.com/tos"
  #define PRIVACY_POLICY_URL						"http://moodbox.com/privacy"
#else
  #define TERM_OF_USE_URL							"http://risovaska.ru/eula"
  #define PRIVACY_POLICY_URL						"http://risovaska.ru/eula"
#endif

class WidgetVerifier;
class RegistrationWidget;

// Registration request
class RegistrationRequest : public ServerRequest
{
	Q_OBJECT

public:
	RegistrationRequest(RegistrationWidget *parent, const UserAccount &userAccount, const QString &invateCode);

signals:
	void registrationRequestCompleted(Fault fault, AccountResultCode::AccountResultCodeEnum result);

private slots:
	void onGetRegistrationRequestResult(QVariant state, Fault fault, AccountResultCode::AccountResultCodeEnum result);
};

// New user registration 
class RegistrationWidget : public ServerWidget, public RegistrationWidgetClass
{
	Q_OBJECT

public:
	RegistrationWidget(QWidget *parent = NULL);

	const QString getLogin() { return loginEdit->text(); };
	const QString getPassword() { return passwordEdit->text(); };
	void clearData();

	void setInviteCode(QString invite) { inviteCode = invite; };

public slots:
	void onCreateAccountResult(Fault fault, AccountResultCode::AccountResultCodeEnum result);

	virtual void onRequestCancelled();

signals:
	void back();
	void registrationSuccess();

	void waitingStart();
	void waitingHide();

protected:
	void registrationError(AccountResultCode::AccountResultCodeEnum result);

private:
	WidgetVerifier *loginVerifier, *passwordsVerifier, *emailVerifier, *iAcceptVerifier;

	UserAccount userAccount;
	RegistrationRequest *currentRequest;

	QString inviteCode;

	void iAcceptHighlight(WidgetVerifier::TextFormat format);

private slots:
	void onNextLinkAction();
	void onBackLinkAction();

	void on_termOfUseButton_clicked();
	void on_privacyPolicyButton_clicked();
};

}

#endif // REGISTRATIONWIDGET_H
