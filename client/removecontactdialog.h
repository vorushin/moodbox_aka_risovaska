#ifndef REMOVECONTACTDIALOG_H
#define REMOVECONTACTDIALOG_H

#include <QDialog>

#include "ui_removecontactdialog.h"
#include "formblocker.h"
#include "contactresultcode.h"
#include "changeuserchannelresult.h"
#include "serverrequest.h"
#include "servercontrol.h"

namespace MoodBox
{

#define REMOVE_ERROR_TITLE			QT_TRANSLATE_NOOP("MoodBox::RemoveContactDialog", "ContactRemovalErrorTitle%1")
#define	REMOVE_USER_LABEL_TEXT		QT_TRANSLATE_NOOP("MoodBox::RemoveContactDialog", "RemoveUserLabel%1")
#define	REMOVE_CHANNEL_LABEL_TEXT	QT_TRANSLATE_NOOP("MoodBox::RemoveContactDialog", "RemoveChannelLabel%1")

using namespace Ui;

class RemoveContactDialog;

// Remove contact request
class RemoveContactRequest : public ServerRequest
{
	Q_OBJECT

public:
	RemoveContactRequest(RemoveContactDialog *parent, ContactType::ContactTypeEnum type, const qint32 &id);

signals:
	void removeFriendRequestCompleted(Fault fault, ContactResultCode::ContactResultCodeEnum result);
	void removeChannelRequestCompleted(Fault fault, ChangeUserChannelResult::ChangeUserChannelResultEnum result);

private:
	qint32 contactId;

private slots:
	void onGetRemoveFriendRequestResult(QVariant state, Fault fault, ContactResultCode::ContactResultCodeEnum result);
	void onGetRemoveChannelRequestResult(QVariant state, Fault fault, ChangeUserChannelResult::ChangeUserChannelResultEnum result);
};

// Remove contact dialog
class RemoveContactDialog : public ServerDialog, public RemoveContactDialogClass
{
	Q_OBJECT

public:
	RemoveContactDialog(const qint32 &id, ContactType::ContactTypeEnum type, QWidget *parent = NULL);

public slots:
	void onGetRemoveFriendResult(Fault fault, ContactResultCode::ContactResultCodeEnum result);
	void onGetRemoveChannelResult(Fault fault, ChangeUserChannelResult::ChangeUserChannelResultEnum result);

	virtual void onRequestCancelled();

private:
	qint32 id;
	ContactType::ContactTypeEnum contactType;
	RemoveContactRequest *currentRequest;

private slots:
	void on_removeButton_clicked();
};

}

#endif // REMOVECONTACTDIALOG_H
