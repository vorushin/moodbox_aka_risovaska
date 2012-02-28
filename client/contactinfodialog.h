#ifndef CONTACTINFOWIDGET_H
#define CONTACTINFOWIDGET_H

#include <QDialog>

#include "ui_contactinfodialog.h"
#include "userinfo.h"
#include "channelresult.h"
#include "userpictureresult.h"
#include "fault.h"
#include "serverrequest.h"
#include "servercontrol.h"

namespace MoodBox
{

using namespace Ui;

// Title
#define INFO_TITLE						QT_TRANSLATE_NOOP("MoodBox::ContactInfoDialog", "Title%1")

// Gender tool tip
#define FEMALE_TOOLTIP					QT_TRANSLATE_NOOP("MoodBox::ContactInfoDialog", "FemaleHint")
#define MALE_TOOLTIP					QT_TRANSLATE_NOOP("MoodBox::ContactInfoDialog", "MaleHint")

#define AGE_LABEL						QT_TRANSLATE_NOOP("MoodBox::ContactInfoDialog", "AgeLabel%1")

#define COUNTRY_LABEL					"(%1)"

// Gender Icons
#define FEMALE_ICON						":/MoodBox/Resources/female_icon.png"
#define MALE_ICON						":/MoodBox/Resources/male_icon.png"

class ContactInfoDialog;

// Contact info request
class ContactInfoRequest : public ServerRequest
{
	Q_OBJECT

public:
	ContactInfoRequest(ContactInfoDialog *parent, QString login);
	ContactInfoRequest(ContactInfoDialog *parent, qint32 id, ContactType::ContactTypeEnum type);
	
signals:
	void userInfoRequestCompleted(Fault fault, UserInfo userInfo);
	void channelInfoRequestCompleted(Fault fault, ChannelResult channelInfo);

private slots:
	void onUserInfoRequestResult(QVariant state, Fault fault, UserInfo userInfo);
	void onChannelInfoRequestResult(QVariant state, Fault fault, ChannelResult channelInfo);
};

class ContactAvatarButton;
class ContactInfo;

// Information about contact
class ContactInfoDialog : public ServerDialog, public ContactInfoDialogClass
{
	Q_OBJECT

public:
	ContactInfoDialog(QString &login, QWidget *parent = NULL);
	ContactInfoDialog(ContactInfo &contactInfo, QWidget *parent = NULL);
	ContactInfoDialog(UserInfo &userInfo, QWidget *parent = NULL, bool isSpecialEntry = false);

public slots:
	void onUserInfoRequestCompleted(Fault fault, UserInfo userInfo);
	void onChannelInfoRequestCompleted(Fault fault, ChannelResult channelInfo);

	virtual void onRequestCancelled();

protected:
	void init(UserInfo &userInfo, bool isSpecialEntry = false, bool isByLogin = false);

	virtual void retranslate();
	virtual void mousePressEvent(QMouseEvent *event);

private:
	UserInfo currentContact;
	bool isByLogin;

	bool addAsFriendButtonVisible;

	ContactInfoRequest *currentInfoRequest;

	ContactType::ContactTypeEnum type;

	void updateInfo();

private slots:
	void on_nameLinkButton_clicked();
	void on_addAsFriendButton_clicked();

	void onNewPictureLoaded(const QString &key);
	void loadPicture();

};

}

#endif // CONTACTINFOWIDGET_H
