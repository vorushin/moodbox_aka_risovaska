#ifndef AUTHORIZATIONDIALOG_H
#define AUTHORIZATIONDIALOG_H

#include <QDialog>
#include <QString>
#include <QSettings>
#include <QHash>

#include "ui_authorizationdialog.h"
#include "contactinfo.h"
#include "userinfo.h"
#include "authorizationresultcode.h"
#include "fault.h"
#include "serverrequest.h"
#include "servercontrol.h"

namespace MoodBox
{

using namespace Ui;

#define REQUEST_TITLE			QT_TRANSLATE_NOOP("MoodBox::AuthorizationDialog", "RequestTitle%1")
#define REQUEST_TOP_LABEL		QT_TRANSLATE_NOOP("MoodBox::AuthorizationDialog", "RequestTopLabel%1")
#define REQUEST_DEFAULT_TEXT	QT_TRANSLATE_NOOP("MoodBox::AuthorizationDialog", "RequestDefaultText")
#define REQUEST_OK_BUTTON		QT_TRANSLATE_NOOP("MoodBox::AuthorizationDialog", "RequestSendButton")
#define REQUEST_CANCEL_BUTTON	QT_TRANSLATE_NOOP("MoodBox::AuthorizationDialog", "RequestCancelButton")

#define APPROVE_TITLE			QT_TRANSLATE_NOOP("MoodBox::AuthorizationDialog", "ApproveTitle%1")
#define APPROVE_TOP_LABEL		QT_TRANSLATE_NOOP("MoodBox::AuthorizationDialog", "ApproveTopLabel%1")
#define APPROVE_ADD_BUTTON		QT_TRANSLATE_NOOP("MoodBox::AuthorizationDialog", "ApproveAddButton")
#define APPROVE_NO_BUTTON		QT_TRANSLATE_NOOP("MoodBox::AuthorizationDialog", "ApproveNoButton")
#define APPROVE_LATER_BUTTON	QT_TRANSLATE_NOOP("MoodBox::AuthorizationDialog", "ApproveLaterButton")

#define REJECTED_CODE					2

#define MAX_REQUEST_MESSAGES_COUNT		25

class FormBlocker;
class AuthorizationDialog;

// Authorization request
class AuthorizationRequest : public ServerRequest
{
	Q_OBJECT

public:
	AuthorizationRequest(AuthorizationDialog *parent, const UserInfo &userInfo, const QString &message);
	AuthorizationRequest(AuthorizationDialog *parent, const qint32 &id, bool approve);	

signals:
	void authorizationRequestCompleted(Fault fault, AuthorizationResultCode::AuthorizationResultCodeEnum result);
	void authorizationResponseCompleted(Fault fault, AuthorizationResultCode::AuthorizationResultCodeEnum result, bool approve);

private:
	UserInfo userInfo;

	bool approve;
	qint32 contactId;

private slots:
	void onGetAuthorizationRequestResult(QVariant state, Fault fault, AuthorizationResultCode::AuthorizationResultCodeEnum result);
	void onGetAuthorizationResponseResult(QVariant state, Fault fault, AuthorizationResultCode::AuthorizationResultCodeEnum result);
};

// Request/response authorization dialog
class AuthorizationDialog : public ServerDialog, public AuthorizationDialogClass
{
	Q_OBJECT

public:
	AuthorizationDialog(QWidget *parent = NULL);

	inline const ContactInfo& getContactInfo() const { return contactInfo; };

	inline void setMessageText(const QString &message) { messageEdit->setPlainText(message); };
	inline QString getMessageText() const { return messageEdit->toPlainText(); };

	void request(const QString &loginInfo);	
	void request(const ContactInfo &contactInfo);
	void request(const UserInfo &userInfo);

	void approve(const ContactInfo &contactInfo);

public slots:
	void onGetAuthorizationRequestCompleted(Fault fault, AuthorizationResultCode::AuthorizationResultCodeEnum result);
	void onGetAuthorizationResponseCompleted(Fault fault, AuthorizationResultCode::AuthorizationResultCodeEnum result, bool approve);

	virtual void onRequestCancelled();

private:
	enum Mode {None, Request, Response};

	ContactInfo contactInfo;
	UserInfo userInfo;

	Mode mode;

	AuthorizationRequest *currentRequest;

	QHash<int, QString> requestMessages;
	bool isMessageExists;
	bool isInternalChanges;
	QString previousMessage;

	void loadMessages();
	void addMessage(const QString &newMessage);
	void saveMessages() const;

private slots:
	void on_okButton_clicked();
	void on_rejectButton_clicked();
	void on_cancelButton_clicked();
	
	void onContactAvatarClicked();

	void checkMessages();
};

}

#endif // AUTHORIZATIONDIALOG_H
