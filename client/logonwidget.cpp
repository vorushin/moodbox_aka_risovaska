#include "logonwidget.h"

#include <QMessageBox>
#include <QSettings>

#include "debug.h"
#include "formblocker.h"
#include "uitools.h"
#include "faulttools.h"
#include "testtools.h"

namespace MoodBox 
{

// LogonRequest class
LogonRequest::LogonRequest(LogonWidget *parent, const QString &login, const QString &password) 
	: ServerRequest()
{
	// Connect parent update
	connect(this, SIGNAL(logonRequestCompleted(Fault, AuthTicketResult)), 
			parent, SLOT(onLogonResult(Fault, AuthTicketResult)));

	SERVER->logon(CALLBACK(this, onLogonRequestResult, AuthTicketResult), QVariant(), login, password);
}

void LogonRequest::onLogonRequestResult(QVariant state, Fault fault, AuthTicketResult result)
{
	Q_UNUSED(state)
	
	if (active)
		emit logonRequestCompleted(fault, result);

	deleteLater();
}

// IsInviteNeedRequest class
InviteNeedRequest::InviteNeedRequest(LogonWidget *parent)
	: ServerRequest()
{
	// Connect parent update
	connect(this, SIGNAL(inviteNeedRequestCompleted(Fault, ServerInfo)), parent, SLOT(onInviteNeedResult(Fault, ServerInfo)));

	SERVER->getServerInfo(CALLBACK(this, onInviteNeedRequestResult, ServerInfo), QVariant());
}

void InviteNeedRequest::onInviteNeedRequestResult(QVariant state, Fault fault, ServerInfo result)
{
	Q_UNUSED(state)
	
	if (active)
		emit inviteNeedRequestCompleted(fault, result);

	deleteLater();
}

// LogonWidget class
LogonWidget::LogonWidget(QWidget *parent)
	: ServerWidget(parent), currentRequest(NULL), inviteNeedRequest(NULL)
{
	TimeMeasure t("LogonWidget");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	// Validators
	loginVerifier = new ComboBoxVerifier(true, this, loginLabel, StandardVerifier::Login);
    passwordVerifier = new PasswordsVerifier(true, this, passwordLabel);

	formVerifier->addVerifier(loginVerifier);
	formVerifier->addVerifier(passwordVerifier);
	
	connect(signInButton, SIGNAL(clicked()), this, SLOT(onSignInAction()));
	connect(passwordEdit, SIGNAL(returnPressed()), this, SLOT(onSignInAction()));
	connect(forgotPasswordButton, SIGNAL(clicked()), this, SLOT(onForgotPasswordAction()));
	connect(signUpButton, SIGNAL(clicked()), this, SLOT(onRegisterAction()));
	connect(settingsButton, SIGNAL(clicked()), this, SLOT(onSettingsAction()));

	connect(loginCombo->lineEdit(), SIGNAL(returnPressed()), this, SLOT(onSignInAction()));
	connect(loginCombo, SIGNAL(currentIndexChanged(const QString&)), this, SLOT(onLoginChoosen(const QString&)));

	timer.setSingleShot(true);
	connect(&timer, SIGNAL(timeout()), this, SLOT(callLogon()));
    
#ifdef Q_WS_MAC
    passwordEdit->setAttribute(Qt::WA_MacShowFocusRect, false);
#endif    
}

void LogonWidget::start(bool alreadyShown)
{
	if(!alreadyShown)
		prepareToShow(true);

	if(LOGONPROVIDER->isAutoLogonPossible())
	{
		onSignInAction();
	}
}

void LogonWidget::cancel()
{
	timer.stop();

	if (currentRequest != NULL)
		currentRequest->detach();

	if (inviteNeedRequest != NULL)
		inviteNeedRequest->detach();
}

void LogonWidget::relogon()
{
	callLogon();
}

void LogonWidget::setLoginAndPassword(const QString &login, const QString &password)
{
	loginCombo->lineEdit()->setText(login);

	loginCombo->lineEdit()->selectAll();
	loginCombo->lineEdit()->setFocus();

	passwordEdit->setText(password);
}

void LogonWidget::onLogonResult(Fault fault, AuthTicketResult result) 
{
	currentRequest = NULL;
	
	if (!fault.isNull())
	{
		switch (FaultTools::getFaultReaction(fault))
		{
			case FaultTools::RetryIfPossible:
				retry();
				break;

			case FaultTools::RequestCannotBeSent:
				Q_ASSERT_X(false, "LogonWidget::onLogonResult", "got RequestCannotBeSent fault reaction");
			case FaultTools::LongRetryIfPossible:
				longRetry();
				break;

			case FaultTools::StopRequests:
				// do nothing:
				// - anyway outer code will move us to correct state
				// - we could got old cancelled request here and must not provide any reaction on it
				break;
		}

		return;
	}

	if (result.getResultCode() != AuthTicketResultCode::Ok)
	{
		switch (result.getResultCode()) 
		{
			case AuthTicketResultCode::InvalidCredentials:
				UiTools::handleError(this, tr(LOGON_FAILED_TITLE), tr(LOGON_FAILED_INVALID_CREDENTIALS));
				break;

			case AuthTicketResultCode::LockedTooManyAttempts:
				UiTools::handleError(this, tr(LOGON_FAILED_TITLE), tr(LOGON_FAILED_TOO_MANY_ATTEMPTS).arg(result.getLockTime()));
				break;

			default:
				UiTools::handleError(this, tr(LOGON_FAILED_TITLE), tr(LOGON_FAILED_UNKNOWN_RESPONSE));
				break;
		};

		emit logonStopped();
		return;
	}

	LOGONPROVIDER->saveLogonDataIfAllowed();

	QDEBUG("Ok, we logged on");

	emit successLogon();
}

void LogonWidget::onInviteNeedResult(Fault fault, ServerInfo result)
{
	inviteNeedRequest = NULL;

	emit waitingStop();

	if (!fault.isNull())
	{
		UiTools::handleError(this, tr(SERVER_ERROR_TITLE), fault);
		return;
	}

	if (result.getIsInvitationRequired())
		emit inviteCodeNeeded();
	else
		emit registerNewUser();
}

void LogonWidget::onRequestCancelled()
{
	cancel();
	emit logonStopped();
}

void LogonWidget::onLoginChoosen(const QString& login)
{
	if(login.isEmpty())
		passwordEdit->clear();
	else
		passwordEdit->setText(LOGONPROVIDER->getPassword(login));
}

void LogonWidget::on_autoLogonCheckBox_stateChanged(int state)
{
	bool value = (state == Qt::Checked);
	LOGONPROVIDER->setIsAutoLogonEnabled(value);
}

void LogonWidget::on_savePasswordCheck_stateChanged(int state)
{
	bool value = (state == Qt::Checked);
	LOGONPROVIDER->setIsPasswordSavingEnabled(value);

	updateAutoLogonCheckBox();
}

void LogonWidget::onForgotPasswordAction()
{
   emit forgotPassword();
}

void LogonWidget::onRegisterAction()
{
	emit waitingStart();

	inviteNeedRequest = new InviteNeedRequest(this);
}

void LogonWidget::onSettingsAction()
{
	emit showSettings();
}

void LogonWidget::onSignInAction()
{
    if (!formVerifier->verifyAndHighlight(false))
		return;

	emit waitingStart();

	LOGONPROVIDER->setCurrent(loginCombo->currentText(), passwordEdit->text());
	
	LOGONPROVIDER->saveStatus(UserStatus::Online, true);

	callLogon();
}

void LogonWidget::onPasswordChangeAction()
{
   emit changePassword();
}

void LogonWidget::callLogon() 
{
	QString login = LOGONPROVIDER->getCurrentLogin();

	emit logonStarted();
	currentRequest = new LogonRequest(this, login, LOGONPROVIDER->getCurrentPassword());
}


void LogonWidget::prepareToShow(bool updateFields)
{
	LOGONPROVIDER->reload();

	savePasswordCheck->setChecked(LOGONPROVIDER->getIsPasswordSavingEnabled());
	autoLogonCheckBox->setChecked(LOGONPROVIDER->getIsAutoLogonEnabled());

	QString oldLogin = loginCombo->lineEdit()->text();
	QString oldPassword = passwordEdit->text();

	loginCombo->clear();
	loginCombo->addItem(QString());
	QStringList logins = LOGONPROVIDER->getSavedLogins();
	QString login;
	foreach(login, logins)
		loginCombo->addItem(login);

	if(updateFields)
	{
		login = LOGONPROVIDER->getLastLoggedOnLogin();
		if (!login.isEmpty()) 
		{
			loginCombo->lineEdit()->setText(login);

			loginCombo->lineEdit()->selectAll();
			loginCombo->lineEdit()->setFocus();

			passwordEdit->setText(LOGONPROVIDER->getPassword(login));
		}
		else
		{
			signUpButton->setStyleSheet("QPushButton{color: #FFC600;} QPushButton:hover{color: #FF8A01;}");
		}
	}
	else
	{
		loginCombo->lineEdit()->setText(oldLogin);
		passwordEdit->setText(oldPassword);
	}

	oldPassword.clear();
}

void LogonWidget::updateAutoLogonCheckBox()
{
	if(savePasswordCheck->isChecked())
		autoLogonCheckBox->setEnabled(true);
	else
	{
		autoLogonCheckBox->setCheckState(Qt::Unchecked);
		autoLogonCheckBox->setEnabled(false);
	}
}

void LogonWidget::retry(int delay)
{
	timer.start(delay + random.getNextIntRanged(0, 2*LOGON_DELAY_DEVIATION) - LOGON_DELAY_DEVIATION);
}

void LogonWidget::longRetry()
{
	retry(LONG_LOGON_DELAY);
}

}