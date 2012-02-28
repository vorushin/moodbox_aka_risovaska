#include "registrationwidget.h"

#include <QMessageBox>
#include <QLocale>
#include <QUrl>
#include <QDesktopServices>

#include "formblocker.h"
#include "verifiers.h"
#include "uitools.h"
#include "serverproxysingleton.h"
#include "international.h"
#include "testtools.h"

namespace MoodBox
{

// RegistrationRequest class
RegistrationRequest::RegistrationRequest(RegistrationWidget *parent, const UserAccount &userAccount, const QString &invateCode)
	: ServerRequest()
{
	connect(this, SIGNAL(registrationRequestCompleted(Fault, AccountResultCode::AccountResultCodeEnum)), parent, SLOT(onCreateAccountResult(Fault, AccountResultCode::AccountResultCodeEnum)));
	
	SERVER->createAccount(CALLBACK(this, onGetRegistrationRequestResult, AccountResultCode::AccountResultCodeEnum), QVariant(), userAccount, invateCode);
}

void RegistrationRequest::onGetRegistrationRequestResult(QVariant state, Fault fault, AccountResultCode::AccountResultCodeEnum result)
{
	Q_UNUSED(state)
	
	if (active)
		emit registrationRequestCompleted(fault, result);

	deleteLater();
}

// RegistrationWidget class
RegistrationWidget::RegistrationWidget(QWidget *parent)
	: ServerWidget(parent), currentRequest(NULL)
{
	TimeMeasure t("RegistrationWidget");

	setupUi(this);

	t.showTimePassedAfterSetupUi();
	
	labelMandatoryFields->setText(tr(MANDATORY_FIELDS));
	
	connect(nextButton, SIGNAL(clicked()), this, SLOT(onNextLinkAction()));
	connect(backButton, SIGNAL(clicked()), this, SLOT(onBackLinkAction()));
	connect(emailEdit, SIGNAL(returnPressed()), this, SLOT(onNextLinkAction()));
	connect(nameEdit, SIGNAL(returnPressed()), this, SLOT(onNextLinkAction()));
	connect(loginEdit, SIGNAL(returnPressed()), this, SLOT(onNextLinkAction()));
	connect(passwordEdit, SIGNAL(returnPressed()), this, SLOT(onNextLinkAction()));
	connect(password2Edit, SIGNAL(returnPressed()), this, SLOT(onNextLinkAction()));
	
	// Verifiers
	loginVerifier = new LineEditVerifier(true, this, loginLabel, StandardVerifier::Login);
	formVerifier->addVerifier(loginVerifier);

	passwordsVerifier = new PasswordsVerifier(true, this, passwordLabel, password2Label);
	formVerifier->addVerifier(passwordsVerifier);

	emailVerifier = new LineEditVerifier(true, this, emailLabel, StandardVerifier::Email);
	formVerifier->addVerifier(emailVerifier);

	userAccount = UserAccount::empty();
    
#ifdef RUSSIAN_VERSION
	andLabel->hide();
	privacyPolicyButton->hide();
#endif
}

void RegistrationWidget::clearData()
{
	nameEdit->clear(); 
	loginEdit->clear(); 
	passwordEdit->clear();
	password2Edit->clear();
	emailEdit->clear();
	receiveNewsCheckBox->setChecked(true);
	iAcceptCheckBox->setChecked(false);

	formVerifier->highlight(WidgetVerifier::Normal);
	iAcceptHighlight(WidgetVerifier::Normal);
}

void RegistrationWidget::onCreateAccountResult(Fault fault, AccountResultCode::AccountResultCodeEnum result)
{
	currentRequest = NULL;
	
	if (!fault.isNull())
	{
		emit waitingHide();

		QString str = fault.getDescription();

		UiTools::handleError(this, tr(REGISTRATION_ERROR_TITLE), fault);

		return;
	}
	
	if (result != AccountResultCode::Ok)
	{
		emit waitingHide();
		registrationError(result);	
	}
	else
	{			
		emit registrationSuccess();
	}
}

void RegistrationWidget::onRequestCancelled()
{
	if (currentRequest != NULL)
		currentRequest->detach();
}

void RegistrationWidget::iAcceptHighlight(WidgetVerifier::TextFormat format)
{
	if (format != WidgetVerifier::Normal)
		QMessageBox::warning(this, tr(MUST_ACCEPT_TOS_TITLE), tr(MUST_ACCEPT_TOS_ERROR));
}

void RegistrationWidget::onNextLinkAction()
{
	if (!formVerifier->verifyAndHighlight(true))
		return;

	if (!iAcceptCheckBox->isChecked())
	{
		iAcceptHighlight(WidgetVerifier::Error);
		return;
	}

	formVerifier->highlight(WidgetVerifier::Normal);
	iAcceptHighlight(WidgetVerifier::Normal);

	userAccount.setLogin(loginEdit->text());
	userAccount.setPassword(passwordEdit->text());
	userAccount.setName(nameEdit->text());
	userAccount.setEmail(emailEdit->text());
	userAccount.setAllowNews(receiveNewsCheckBox->isChecked());
	userAccount.setAllowShowFriends(true); // by default it's possible to see list of friends
	userAccount.setAllowPublishing(true); // by default publishing all messages in "All Friends"
	
	emit waitingStart();

	currentRequest = new RegistrationRequest(this, userAccount, inviteCode);
}

void RegistrationWidget::onBackLinkAction()
{
	emit back();
	clearData();
}

void RegistrationWidget::on_termOfUseButton_clicked()
{
	static QUrl termOfUseUrl = QUrl(TERM_OF_USE_URL);
	QDesktopServices::openUrl(termOfUseUrl);
}

void RegistrationWidget::on_privacyPolicyButton_clicked()
{
	static QUrl privacyPolicyUrl = QUrl(PRIVACY_POLICY_URL);
	QDesktopServices::openUrl(privacyPolicyUrl);
}

void RegistrationWidget::registrationError(AccountResultCode::AccountResultCodeEnum result)
{
	QString errorMessage = tr(REGISTRATION_ERROR);
	
	switch (result)
	{
		case AccountResultCode::InvalidLogin: 
			errorMessage = tr(INVALID_LOGIN_ERROR_MESSAGE);
			loginVerifier->highlight(WidgetVerifier::Error);
			loginEdit->setFocus();
			break;

		case AccountResultCode::LoginIsNotAvailable: 
			errorMessage = tr(LOGIN_IS_NOT_AVAILABLE_ERROR_MESSAGE);
			loginVerifier->highlight(WidgetVerifier::Error);
			loginEdit->setFocus();
			break;

		case AccountResultCode::InvalidPassword: 
			errorMessage = tr(CHECK_PASSWORD_ERROR_MESSAGE);
			passwordsVerifier->highlight(WidgetVerifier::Error);
			passwordEdit->setFocus();
			break;

		case AccountResultCode::InvalidEmail: 
			errorMessage = tr(CHECK_EMAIL_ERROR_MESSAGE);
			emailVerifier->highlight(WidgetVerifier::Error);
			emailEdit->setFocus();
			break;
	}

	QMessageBox::warning(this, tr(REGISTRATION_ERROR_TITLE), errorMessage);
}

}