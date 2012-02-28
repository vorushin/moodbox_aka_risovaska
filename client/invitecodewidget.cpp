#include "invitecodewidget.h"

#include "uitools.h"
#include "formblocker.h"
#include "international.h"
#include "testtools.h"

namespace MoodBox
{

// InviteCodeRequest class
InviteCodeRequest::InviteCodeRequest(InviteCodeWidget *parent, const QString &inviteCode) 
	: ServerRequest()
{
	// Connect parent update
	connect(this, SIGNAL(inviteCodeRequestCompleted(Fault, InvitationResultCode::InvitationResultCodeEnum)), parent, SLOT(onCheckInvitationResult(Fault, InvitationResultCode::InvitationResultCodeEnum)));

	SERVER->checkInvitation(CALLBACK(this, onInviteCodeRequestResult, InvitationResultCode::InvitationResultCodeEnum), QVariant(), inviteCode);
}

void InviteCodeRequest::onInviteCodeRequestResult(QVariant state, Fault fault, InvitationResultCode::InvitationResultCodeEnum result)
{
	Q_UNUSED(state)
	
	if (active)
		emit inviteCodeRequestCompleted(fault, result);

	deleteLater();
}

// InviteCodeWidget class
InviteCodeWidget::InviteCodeWidget(QWidget *parent)
	: ServerWidget(parent), currentRequest(NULL)
{
	TimeMeasure t("InviteCodeWidget");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	formVerifier->addVerifier(new LineEditVerifier(true, this, inviteCodeLabel, StandardVerifier::NotEmpty));

	connect(inviteCodeEdit, SIGNAL(returnPressed()), nextButton, SIGNAL(clicked()));
	connect(nextButton, SIGNAL(clicked()), this, SLOT(onNextLinkAction()));
	connect(backButton, SIGNAL(clicked()), this, SLOT(onBackLinkAction()));
}

void InviteCodeWidget::onCheckInvitationResult(Fault fault, InvitationResultCode::InvitationResultCodeEnum result)
{
	currentRequest = NULL;
	emit waitingStop();

	if (!fault.isNull())
	{
		UiTools::handleError(this, tr(INVALID_INVITE_CODE_ERROR_TITLE), fault);
		return;
	}

	if (result != InvitationResultCode::Ok) 
	{
		UiTools::handleError(QApplication::activeWindow(), tr(INVALID_INVITE_CODE_ERROR_TITLE), tr(INVALID_INVITE_CODE_ERROR_TEXT));
		return;
	}

	emit next();
}

void InviteCodeWidget::clearData()
{
	inviteCodeEdit->clear();
}

void InviteCodeWidget::onRequestCancelled()
{
	if (currentRequest != NULL)
		currentRequest->detach();
}

void InviteCodeWidget::onNextLinkAction()
{
	if (!formVerifier->verifyAndHighlight())
		return;

	emit waitingStart();
    
	currentRequest = new InviteCodeRequest(this, inviteCodeEdit->text());
}

void InviteCodeWidget::onBackLinkAction()
{
	emit back();
}

}
