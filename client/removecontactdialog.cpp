#include "removecontactdialog.h"

#include <QString>

#include "uitools.h"
#include "international.h"
#include "peopleinfomanager.h"
#include "testtools.h"

namespace MoodBox
{

// RemoveContactRequest class
RemoveContactRequest::RemoveContactRequest(RemoveContactDialog *parent, ContactType::ContactTypeEnum type, const qint32 &id)
	: ServerRequest()
{
	contactId = id;
	switch (type) 
	{
		case ContactType::Friend: 
			connect(this, SIGNAL(removeFriendRequestCompleted(Fault, ContactResultCode::ContactResultCodeEnum)), parent, SLOT(onGetRemoveFriendResult(Fault, ContactResultCode::ContactResultCodeEnum)));
			SERVER->removeFromContacts(CALLBACK(this, onGetRemoveFriendRequestResult, ContactResultCode::ContactResultCodeEnum), id, id);
			break;

		case ContactType::Channel:
			connect(this, SIGNAL(removeChannelRequestCompleted(Fault, ChangeUserChannelResult::ChangeUserChannelResultEnum)), parent, SLOT(onGetRemoveChannelResult(Fault, ChangeUserChannelResult::ChangeUserChannelResultEnum)));
			SERVER->deleteUserFromChannel(CALLBACK(this, onGetRemoveChannelRequestResult, ChangeUserChannelResult::ChangeUserChannelResultEnum), id, id);
	}
}

void RemoveContactRequest::onGetRemoveFriendRequestResult(QVariant state, Fault fault, ContactResultCode::ContactResultCodeEnum result)
{
	Q_UNUSED(state)
	
	if (fault.isNull())
	{
		if (result == ContactResultCode::Ok && INFOMANAGER->isUserOnline())
			INFOMANAGER->removeFromContactList(contactId);
	}

	if (active)
		emit removeFriendRequestCompleted(fault, result);

	deleteLater();
}

void RemoveContactRequest::onGetRemoveChannelRequestResult(QVariant state, Fault fault, ChangeUserChannelResult::ChangeUserChannelResultEnum result)
{
	Q_UNUSED(state)
	
	if (fault.isNull())
	{
		if (result == ChangeUserChannelResult::Ok && INFOMANAGER->isUserOnline())
			INFOMANAGER->removeFromContactList(contactId);
	}

	if (active)
		emit removeChannelRequestCompleted(fault, result);

	deleteLater();
}

// RemoveContactDialog class
RemoveContactDialog::RemoveContactDialog(const qint32 &id, ContactType::ContactTypeEnum type, QWidget *parent)
	: ServerDialog(parent), currentRequest(NULL)
{
	TimeMeasure t("RemoveContactDialog");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	UiTools::moveWindowToScreenCenter(this);

	this->id = id;

	// Blocker
	formBlocker->addWidget(removeButton);
	formBlocker->addWidget(cancelButton);

	contactType = type;
	switch (type) 
	{
		case ContactType::Friend: 
			removeLabel->setText(tr(REMOVE_USER_LABEL_TEXT).arg(INFOMANAGER->getContact(id)->getDisplayName()));
			break;

		case ContactType::Channel: 
			removeLabel->setText(tr(REMOVE_CHANNEL_LABEL_TEXT).arg(INFOMANAGER->getContact(id)->getDisplayName()));
			break;
	}	

	connect(cancelButton, SIGNAL(clicked()), this, SLOT(close()));
	connect(closeToolButton, SIGNAL(clicked()), this, SLOT(reject()));
}

void RemoveContactDialog::onGetRemoveFriendResult(Fault fault, ContactResultCode::ContactResultCodeEnum result)
{
	currentRequest = NULL;
	formBlocker->unblock();

	if (!fault.isNull())
	{
		UiTools::handleError(this, tr(REMOVE_ERROR_TITLE).arg(INFOMANAGER->getContact(id)->getDisplayName()), fault);
		return;
	}
		
	if (result != ContactResultCode::Ok)
	{
		QString error = PromptHelper::getRemoveFriendErrorName(result).arg(INFOMANAGER->getContact(id)->getDisplayName());

		UiTools::handleError(QApplication::activeWindow(), tr(REMOVE_ERROR_TITLE).arg(INFOMANAGER->getContact(id)->getDisplayName()), error);
	}
	else
	{
		close();
	}
}

void RemoveContactDialog::onGetRemoveChannelResult(Fault fault, ChangeUserChannelResult::ChangeUserChannelResultEnum result)
{
	currentRequest = NULL;
	formBlocker->unblock();

	if (!fault.isNull())
	{
		UiTools::handleError(this, tr(REMOVE_ERROR_TITLE).arg(INFOMANAGER->getContact(id)->getDisplayName()), fault);
		return;
	}
		
	if (result != ChangeUserChannelResult::Ok)
	{
		QString error = PromptHelper::getChangeUserChannelErrorName(result).arg(INFOMANAGER->getContact(id)->getDisplayName());

		UiTools::handleError(QApplication::activeWindow(), tr(REMOVE_ERROR_TITLE).arg(INFOMANAGER->getContact(id)->getDisplayName()), error);
	}
	else
	{
		close();
	}
}

void RemoveContactDialog::onRequestCancelled()
{
	if (currentRequest != NULL)
		currentRequest->detach();
}

void RemoveContactDialog::on_removeButton_clicked()
{
	formBlocker->block(true);

	currentRequest = new RemoveContactRequest(this, contactType, id);
}

}