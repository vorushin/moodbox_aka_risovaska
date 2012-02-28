#ifndef PROFILEFRAME_H
#define PROFILEFRAME_H

#include "setupdialogframe.h"

#include "ui_profileframe.h"
#include "servercontrol.h"
#include "accountresultcode.h"
#include "useravatarbutton.h"
#include "international.h"
#include "serverrequest.h"
#include "useraccount.h"
#include "fault.h"

namespace MoodBox
{

using namespace Ui;

// Title
#define UPDATE_TITLE				QT_TRANSLATE_NOOP("MoodBox::ProfileFrame", "Title%1")

// Errors
#define UPDATE_ERROR_TITLE			QT_TRANSLATE_NOOP("MoodBox::ProfileFrame", "UpdateErrorTitle")

class ProfileFrame;

// Account update request
class UserAccountUpdateRequest : public ServerRequest
{
	Q_OBJECT

public:
	UserAccountUpdateRequest(ProfileFrame *parent, const UserAccount &account, bool hasUserPicture, const QPixmap userPicture);

signals:
	void accountUpdateCompleted(Fault fault, AccountResultCode::AccountResultCodeEnum result);

private slots:
	void onUpdateAccountResult(QVariant state, Fault fault, AccountResultCode::AccountResultCodeEnum result);

private:
	UserAccount account;
	bool hasUserPicture;
	QPixmap userPicture;
};

// Frame for setup/change profile 
class ProfileFrame : public SetupDialogFrame, public ProfileFrameClass
{
	Q_OBJECT

public:
	ProfileFrame(QWidget *parent);

	virtual bool isValid();

	bool isChanged() { return profileWasChanged; };
	
	bool isAccessible();

	void updateControls();
	virtual void startUpdate();

public slots:
	void onAccountUpdateCompleted(Fault fault, AccountResultCode::AccountResultCodeEnum result);

	virtual void onRequestCancelled();

protected:
	virtual void showEvent(QShowEvent *event);

private:
	bool profileWasChanged;
	bool avatarWasChanged;

	UserAccount userAccount;
	UserAccountUpdateRequest *currentRequest;

	void fillBirthdayControls();
	void clearBirthdayControls();

	void setYearInCombo(int year);
	int getYearFromCombo() const;
	
private slots:
	void onMonthComboChanged();
	void onUserAvatarButtonClicked();
	void onChangePasswordButtonClicked();

	void clearBirthday();
	void profileChanged();
};

}

#endif // PROFILEFRAME_H
