#include "authorizationdialog.h"

#include <QMessageBox>
#include <QCloseEvent>

#include "debug.h"
#include "uitools.h"
#include "peopleinfomanager.h"
#include "avatarbutton.h"
#include "common.h"
#include "contactinfodialog.h"
#include "formblocker.h"
#include "international.h"
#include "testtools.h"

namespace MoodBox
{

// AuthorizationRequest class
AuthorizationRequest::AuthorizationRequest(AuthorizationDialog *parent, const UserInfo &userInfo, const QString &message)
	: ServerRequest()
{
	this->userInfo = userInfo;
	contactId = userInfo.getUserId();
	
	connect(this, SIGNAL(authorizationRequestCompleted(Fault, AuthorizationResultCode::AuthorizationResultCodeEnum)), parent, SLOT(onGetAuthorizationRequestCompleted(Fault, AuthorizationResultCode::AuthorizationResultCodeEnum)));

	if (userInfo.getUserId() >= 0)
		SERVER->processAuthorizationRequest(CALLBACK(this, onGetAuthorizationRequestResult, AuthorizationResultCode::AuthorizationResultCodeEnum), userInfo.getUserId(), message);
	else
		SERVER->processAuthorizationRequestByLogin(CALLBACK(this, onGetAuthorizationRequestResult, AuthorizationResultCode::AuthorizationResultCodeEnum), userInfo.getLogin(), message);

	QDEBUG("AuthorizationRequest sent");
}
		
AuthorizationRequest::AuthorizationRequest(AuthorizationDialog *parent, const qint32 &id, bool approve)
	: ServerRequest()
{
	contactId = id;
	this->approve = approve;

	connect(this, SIGNAL(authorizationResponseCompleted(Fault, AuthorizationResultCode::AuthorizationResultCodeEnum, bool)), parent, SLOT(onGetAuthorizationResponseCompleted(Fault, AuthorizationResultCode::AuthorizationResultCodeEnum, bool)));

	SERVER->processAuthorizationResponse(CALLBACK(this, onGetAuthorizationResponseResult, AuthorizationResultCode::AuthorizationResultCodeEnum), contactId, approve);	
	QDEBUG("AuthorizationResponse sent");
}

void AuthorizationRequest::onGetAuthorizationRequestResult(QVariant state, Fault fault, AuthorizationResultCode::AuthorizationResultCodeEnum result)
{
	Q_UNUSED(state)
	
	QDEBUG("AuthorizationRequest result come");

	if (fault.isNull() && INFOMANAGER->isUserOnline())
	{
		if (contactId < 0)
		{
			if (result == AuthorizationResultCode::Ok)
			{
				INFOMANAGER->reloadData();
			}
		}
		else
		{
			if (result == AuthorizationResultCode::Ok || result == AuthorizationResultCode::AuthorizedByThisRequest)
			{
				ContactInfo tmpInfo(userInfo.getUserId(), userInfo.getLogin(), UserStatus::Undefined, userInfo.getMotto(), userInfo.getName(), userInfo.getBirthDay(), AuthorizationState::NotAuthorizedMe, QString(), false, ContactType::Friend);
				INFOMANAGER->addToContactList(tmpInfo);
			}

			if (result == AuthorizationResultCode::AuthorizedByThisRequest)
			{
				INFOMANAGER->authorizeContact(contactId);
			}
		}
	}

	if (active)
		emit authorizationRequestCompleted(fault, result);

	deleteLater();
}

void AuthorizationRequest::onGetAuthorizationResponseResult(QVariant state, Fault fault, AuthorizationResultCode::AuthorizationResultCodeEnum result)
{
	Q_UNUSED(state)
	
	QDEBUG("AuthorizationReseponse result come");

	if (fault.isNull() && (result == AuthorizationResultCode::Ok || AuthorizationResultCode::AlreadyAuthorized) )
	{
		if (INFOMANAGER->isUserOnline())
		{
			if (approve)
				INFOMANAGER->authorizeContact(contactId);
			else
				INFOMANAGER->removeFromContactList(contactId);
		}
	}

	if (active)
		emit authorizationResponseCompleted(fault, result, approve);

	deleteLater();
}

// AuthorizationDialog class
AuthorizationDialog::AuthorizationDialog(QWidget *parent)
	: ServerDialog(parent), mode(None), currentRequest(NULL), isMessageExists(false), isInternalChanges(false)
{
	TimeMeasure t("AuthorizationDialog");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	UiTools::moveWindowToScreenCenter(this);

	// Blocker
	formBlocker->addWidget(messageEdit);
	formBlocker->addWidget(okButton);
	formBlocker->addWidget(rejectButton);
	formBlocker->addWidget(cancelButton);
	formBlocker->addWidget(contactAvatarToolButton);

	connect(closeToolButton, SIGNAL(clicked()), this, SLOT(reject()));
}

void AuthorizationDialog::request(const QString &loginInfo)
{
	UserInfo tmplInfo(-1, loginInfo, loginInfo, QDateTime(), QString(), QString(), QLocale::AnyCountry, QString(), Sex::Undefined, QDate(), QString(), UserStatus::Undefined);

	request(tmplInfo);
}

void AuthorizationDialog::request(const ContactInfo &contactInfo)
{
	UserInfo tmplInfo(contactInfo.getUserId(), contactInfo.getLogin(), contactInfo.getName(), QDateTime(), contactInfo.getMotto(), QString(), QLocale::AnyCountry, QString(), Sex::Undefined, contactInfo.getBirthDay(), QString(), UserStatus::Undefined);

	request(tmplInfo);
}

void AuthorizationDialog::request(const UserInfo &userInfo)
{
	this->userInfo = userInfo;

	topLabel->setText(tr(REQUEST_TOP_LABEL).arg(userInfo.getDisplayName()));

	messageEdit->setPlainText(tr(REQUEST_DEFAULT_TEXT));
	messageEdit->setReadOnly(false);
	messageEdit->setFocus();
	messageEdit->selectAll();
	
	connect(messageEdit, SIGNAL(textChanged()), this, SLOT(checkMessages()));
	loadMessages();

	okButton->setText(tr(REQUEST_OK_BUTTON));
	rejectButton->hide();
	cancelButton->setText(tr(REQUEST_CANCEL_BUTTON));

	// Set widget info about yourself
	UserAccount currentUser = INFOMANAGER->getUserAccount();
	authUserNameLabel->setText(currentUser.getDisplayName());
	
	contactAvatarToolButton->setThumbnailMode(false);
	contactAvatarToolButton->setContactId(currentUser.getId());

	mode = Request;
	
	show();
}

void AuthorizationDialog::approve(const ContactInfo &contactInfo)
{
	this->contactInfo = contactInfo;

	topLabel->setText(tr(APPROVE_TOP_LABEL).arg(contactInfo.getDisplayName()));
	authUserNameLabel->setText(contactInfo.getDisplayName());

	messageEdit->setPlainText(contactInfo.getMessage());
	messageEdit->setReadOnly(true);

	okButton->setText(tr(APPROVE_ADD_BUTTON));
	rejectButton->setText(tr(APPROVE_NO_BUTTON));
	cancelButton->setText(tr(APPROVE_LATER_BUTTON));

	// Set widget info about person which want to add you
	contactAvatarToolButton->setThumbnailMode(false);
	contactAvatarToolButton->setContactId(contactInfo.getUserId());
	connect(contactAvatarToolButton, SIGNAL(clicked()), this, SLOT(onContactAvatarClicked()));

	mode = Response;

	show();
}

void AuthorizationDialog::onGetAuthorizationRequestCompleted(Fault fault, AuthorizationResultCode::AuthorizationResultCodeEnum result)
{
	currentRequest = NULL;
	formBlocker->unblock();

	if (!fault.isNull())
	{
		UiTools::handleError(this, tr(AUTH_REQUEST_SEND_ERROR_TITLE).arg(userInfo.getDisplayName()), fault);
		return;
	}

	if (result != AuthorizationResultCode::Ok && result != AuthorizationResultCode::AuthorizedByThisRequest)
	{
		QString error = PromptHelper::getAuthorizationErrorName(result);
		
		UiTools::handleError(this, tr(AUTH_REQUEST_SEND_ERROR_TITLE).arg(userInfo.getDisplayName()), error);
	}
	else
	{
		done(QDialog::Accepted);
	}
}

void AuthorizationDialog::onGetAuthorizationResponseCompleted(Fault fault, AuthorizationResultCode::AuthorizationResultCodeEnum result, bool approve)
{
	currentRequest = NULL;
	formBlocker->unblock();

	if (!fault.isNull())
	{
		UiTools::handleError(this, tr(AUTH_RESPONSE_SEND_ERROR_TITLE).arg(contactInfo.getDisplayName()), fault);
		return;
	}

	if (result != AuthorizationResultCode::Ok && result != AuthorizationResultCode::AlreadyAuthorized)
	{
		QString error = PromptHelper::getAuthorizationErrorName(result);
		
		UiTools::handleError(this, tr(AUTH_RESPONSE_SEND_ERROR_TITLE).arg(contactInfo.getDisplayName()), error);
	}
	else
	{
		if (approve)
			done(QDialog::Accepted);
		else
			done(REJECTED_CODE);
	}
}

void AuthorizationDialog::onRequestCancelled()
{
	if (currentRequest != NULL)
		currentRequest->detach();
}

void AuthorizationDialog::on_okButton_clicked()
{
	addMessage(getMessageText());

	if (mode == Request)
		currentRequest = new AuthorizationRequest(this, userInfo, getMessageText());

	if (mode == Response)
		currentRequest = new AuthorizationRequest(this, contactInfo.getUserId(), true);

	formBlocker->block(true);
}

void AuthorizationDialog::on_rejectButton_clicked()
{
	if (mode == Response)
		currentRequest = new AuthorizationRequest(this, contactInfo.getUserId(), false);

	formBlocker->block(true);
}

void AuthorizationDialog::on_cancelButton_clicked()
{
	formBlocker->unblock();
	
	done(QDialog::Rejected);
}

void AuthorizationDialog::onContactAvatarClicked()
{
	ContactInfoDialog *contactDialog = new ContactInfoDialog(contactInfo);
	contactDialog->show();
}

void AuthorizationDialog::loadMessages()
{
	requestMessages.clear();

	QSettings settings(INFOMANAGER->getUserSettingsFolder() + SETTINGS_INI_FILE, QSettings::IniFormat);
	int size = settings.beginReadArray("RequestMessages");

	int messageId;
	QString messageText;
	for (int i = 0; i < size; ++i) 
	{
		settings.setArrayIndex(i);

		messageId = settings.value("id").toInt();
		messageText = settings.value("MessageText").toString();
		requestMessages.insert(messageId, messageText);
	}
	settings.endArray();
}

void AuthorizationDialog::addMessage(const QString &newMessage)
{
	if (!isMessageExists)
	{
		if (requestMessages.count() >= MAX_REQUEST_MESSAGES_COUNT)
		{
			requestMessages.remove(requestMessages.begin().key());
		}
		
		QHash<int, QString>::const_iterator messageIterator = requestMessages.begin();
		int maxKey = messageIterator.key();
		while (messageIterator != requestMessages.end())
		{
			maxKey = messageIterator.key();
			++messageIterator;
		}

		requestMessages.insert(maxKey + 1, newMessage);
		saveMessages();
	}	
}

void AuthorizationDialog::saveMessages() const
{
	QSettings settings(INFOMANAGER->getUserSettingsFolder() + SETTINGS_INI_FILE, QSettings::IniFormat);
	settings.beginWriteArray("RequestMessages");

	QHash<int, QString>::const_iterator messageIterator = requestMessages.begin();
	int i = 0;
	while (messageIterator != requestMessages.end()) 
	{
		settings.setArrayIndex(i);
		settings.setValue("id", messageIterator.key());
		settings.setValue("MessageText", messageIterator.value());
		
		++messageIterator;
		i++;
	}
	settings.endArray();
}

void AuthorizationDialog::checkMessages()
{
	if (isInternalChanges)
		return;

	QString messageText = getMessageText();
	if (previousMessage == messageText)
		return;

	isInternalChanges = true;
	isMessageExists = false;
	if (messageText.length() >= 2)
	{
		QHash<int, QString>::const_iterator messageIterator = requestMessages.begin();
		while (messageIterator != requestMessages.end())
		{
			if (messageIterator.value().startsWith(messageText))
			{
				messageEdit->setPlainText(messageIterator.value());
				QTextCursor textCursor = messageEdit->textCursor();
				textCursor.movePosition(QTextCursor::End);
				textCursor.setPosition(messageText.length(), QTextCursor::KeepAnchor);
				messageEdit->setTextCursor(textCursor);

				isMessageExists = true;
				break;
			}
			++messageIterator;
		}
	}
	previousMessage = messageText;
	isInternalChanges = false;
}

}
