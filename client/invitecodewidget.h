#ifndef INVITECODEWIDGET_H
#define INVITECODEWIDGET_H

#include "servercontrol.h"

#include "ui_invitecodewidget.h"
#include "invitationresultcode.h"
#include "serverrequest.h"

namespace MoodBox
{

using namespace Ui;

#define INVALID_INVITE_CODE_ERROR_TITLE			QT_TRANSLATE_NOOP("MoodBox::InviteCodeWidget", "InviteCodeErrorMessageBoxTitle")
#define INVALID_INVITE_CODE_ERROR_TEXT			QT_TRANSLATE_NOOP("MoodBox::InviteCodeWidget", "InviteCodeErrorMessageBoxText")

class InviteCodeWidget;

// Invite code request
class InviteCodeRequest : public ServerRequest
{
	Q_OBJECT

public:
	InviteCodeRequest(InviteCodeWidget *parent, const QString &inviteCode);
	
signals:
	void inviteCodeRequestCompleted(Fault fault, InvitationResultCode::InvitationResultCodeEnum result);

private slots:
	void onInviteCodeRequestResult(QVariant state, Fault fault, InvitationResultCode::InvitationResultCodeEnum result);
};

// Invite code widget
class InviteCodeWidget : public ServerWidget, public InviteCodeWidgetClass
{
	Q_OBJECT

public:
	InviteCodeWidget(QWidget *parent = NULL);

	QString getInviteCode() { return inviteCodeEdit->text(); };

signals:
	void next();
	void back();

	void waitingStart();
	void waitingStop();

public slots:
	void onCheckInvitationResult(Fault fault, InvitationResultCode::InvitationResultCodeEnum result);

	void clearData();

	virtual void onRequestCancelled();

private:
	InviteCodeRequest *currentRequest;

private slots:
	void onNextLinkAction();
	void onBackLinkAction();
};

}

#endif // INVITECODEWIDGET_H
