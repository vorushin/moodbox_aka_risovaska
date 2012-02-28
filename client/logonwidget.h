#ifndef LOGONWIDGET_H
#define LOGONWIDGET_H

#include <QWidget>
#include <QTimer>

#include "ui_logonwidget.h"
#include "servercontrol.h"
#include "serverproxysingleton.h"
#include "verifiers.h"
#include "moodboxcustomserver.h"
#include "formblocker.h"
#include "serverrequest.h"
#include "mtrandom.h"

#include "logondataprovider.h"
#include "userstatus.h"

namespace MoodBox 
{

#define SERVER_ERROR_TITLE					QT_TRANSLATE_NOOP("MoodBox::LogonWidget", "ServerErrorMessageBoxTitle")

#define LOGON_FAILED_TITLE					QT_TRANSLATE_NOOP("MoodBox::LogonWidget", "LogonFailedMessageBoxTitle")
#define LOGON_FAILED_INVALID_CREDENTIALS	QT_TRANSLATE_NOOP("MoodBox::LogonWidget", "LogonFailedInvalidCredentials")
#define LOGON_FAILED_UNKNOWN_RESPONSE		QT_TRANSLATE_NOOP("MoodBox::LogonWidget", "LogonFailedUnknownServerResponse")
#define LOGON_FAILED_TOO_MANY_ATTEMPTS		QT_TRANSLATE_NOOP("MoodBox::LogonWidget", "LogonFailedTooManyAttempts%1")

#define SHORT_LOGON_DELAY		13000
#define LONG_LOGON_DELAY		30000
#define LOGON_DELAY_DEVIATION	 3000 

using namespace Ui;

class LogonWidget;

// Login request
class LogonRequest : public ServerRequest
{
	Q_OBJECT

public:
	LogonRequest(LogonWidget *parent, const QString &login, const QString &password);
	
signals:
	void logonRequestCompleted(Fault fault, AuthTicketResult result);

private slots:
	void onLogonRequestResult(QVariant state, Fault fault, AuthTicketResult result);
};

// Check if user need to enter invite code
class InviteNeedRequest : public ServerRequest
{
	Q_OBJECT

public:
	InviteNeedRequest(LogonWidget *parent);
	
signals:
	void inviteNeedRequestCompleted(Fault fault, ServerInfo result);

private slots:
	void onInviteNeedRequestResult(QVariant state, Fault fault, ServerInfo result);
};

// LogonWidget class
class LogonWidget : public ServerWidget, public LogonWidgetClass
{
	Q_OBJECT

public:
	LogonWidget(QWidget *parent = NULL);

	void start(bool alreadyShown = false);
	void cancel();
	void relogon();

	void setLoginAndPassword(const QString &login, const QString &password);
	void prepareToShow(bool updateFields = false);

private:
	void updateAutoLogonCheckBox();

signals:
	void forgotPassword();
	void registerNewUser();
	void inviteCodeNeeded();
	void changePassword();
	void showSettings();

	void logonStarted();
	void logonStopped();
	void successLogon();

	void waitingStart();
	void waitingStop();

public slots:
	void onLogonResult(Fault fault, AuthTicketResult result);
	void onInviteNeedResult(Fault fault, ServerInfo result);

	virtual void onRequestCancelled();

private slots:
	void callLogon();

	void onForgotPasswordAction();
	void onSettingsAction();
	void onRegisterAction();
	void onSignInAction();
	void onPasswordChangeAction();
	void onLoginChoosen(const QString& login);
	
	void on_savePasswordCheck_stateChanged(int);
	void on_autoLogonCheckBox_stateChanged(int);

private:
	QTimer timer;
	MTRandom random;
	LogonRequest *currentRequest;
	InviteNeedRequest *inviteNeedRequest;

	WidgetVerifier *loginVerifier, *passwordVerifier;

	void retry(int delay = SHORT_LOGON_DELAY);
	void longRetry();
};

}

#endif // LOGONWIDGET_H
