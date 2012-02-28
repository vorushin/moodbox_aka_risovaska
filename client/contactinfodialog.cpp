#include "contactinfodialog.h"

#include <QCloseEvent>
#include <QUrl>
#include <QDesktopServices>

#include "uitools.h"
#include "peopleinfomanager.h"
#include "contactavatarbutton.h"
#include "contactinfo.h"
#include "international.h"
#include "authorizationdialog.h"
#include "picturefileloader.h"

#include "testtools.h"

namespace MoodBox
{

// ContactInfoRequest class
ContactInfoRequest::ContactInfoRequest(ContactInfoDialog *parent, QString login) 
	: ServerRequest()
{
	// Connect parent update
	connect(this, SIGNAL(userInfoRequestCompleted(Fault, UserInfo)), parent, SLOT(onUserInfoRequestCompleted(Fault, UserInfo)));

	SERVER->getUserInfoByLogin(CALLBACK(this, onUserInfoRequestResult, UserInfo), login);
}

ContactInfoRequest::ContactInfoRequest(ContactInfoDialog *parent, qint32 id, ContactType::ContactTypeEnum type) 
	: ServerRequest()
{
	// Connect parent update
	switch (type)
	{
		case ContactType::Friend:
			connect(this, SIGNAL(userInfoRequestCompleted(Fault, UserInfo)), parent, SLOT(onUserInfoRequestCompleted(Fault, UserInfo)));
			SERVER->getUserInfo(CALLBACK(this, onUserInfoRequestResult, UserInfo), id);
			break;

		case ContactType::Channel:
			connect(this, SIGNAL(channelInfoRequestCompleted(Fault, ChannelResult)), parent, SLOT(onChannelInfoRequestCompleted(Fault, ChannelResult)));
			SERVER->getChannelInfo(CALLBACK(this, onChannelInfoRequestResult, ChannelResult), id);
			break;
	}
}

void ContactInfoRequest::onUserInfoRequestResult(QVariant state, Fault fault, UserInfo userInfo)
{
	Q_UNUSED(state)
	
	if (active)
		emit userInfoRequestCompleted(fault, userInfo);

	deleteLater();
}

void ContactInfoRequest::onChannelInfoRequestResult(QVariant state, Fault fault, ChannelResult channelInfo)
{
	Q_UNUSED(state)
	
	if (active)
		emit channelInfoRequestCompleted(fault, channelInfo);

	deleteLater();
}

// ContactInfoDialog class
ContactInfoDialog::ContactInfoDialog(QString &login, QWidget *parent)
	: ServerDialog(parent, Qt::Popup)
{
	UserInfo userInfo = UserInfo(0, login, QString(), QDateTime(), QString(), QString(), QLocale::AnyCountry, QString(), Sex::Undefined, QDate(), QString(), UserStatus::Undefined);

	type = ContactType::Friend;

	init(userInfo, false, true);
}

ContactInfoDialog::ContactInfoDialog(ContactInfo &contactInfo, QWidget *parent)
	: ServerDialog(parent, Qt::Popup)
{	
	UserInfo userInfo = UserInfo(contactInfo.getUserId(), contactInfo.getLogin(), contactInfo.getName(), QDateTime(), contactInfo.getMotto(), QString(), QLocale::AnyCountry, QString(), Sex::Undefined, contactInfo.getBirthDay(), QString(), UserStatus::Undefined);

	type = contactInfo.getType();

	init(userInfo, false);
}

ContactInfoDialog::ContactInfoDialog(UserInfo &userInfo, QWidget *parent, bool isSpecialEntry)
	: ServerDialog(parent, Qt::Popup)
{
	type = ContactType::Friend;

	init(userInfo, isSpecialEntry);
}

void ContactInfoDialog::onUserInfoRequestCompleted(Fault fault, UserInfo userInfo)
{
	currentInfoRequest = NULL;
	setCursor(Qt::ArrowCursor);

	if (!fault.isNull())
	{
		UiTools::handleError(QApplication::activeWindow(), tr(INFO_GET_ERROR_TITLE).arg(currentContact.getDisplayName()), fault);
		return;
	}

	if(!userInfo.isNull())
	{
		currentContact = userInfo;

		loadPicture();

		updateInfo();

		addAsFriendButton->setVisible(addAsFriendButtonVisible);
	}
}

void ContactInfoDialog::onChannelInfoRequestCompleted(Fault fault, ChannelResult channelInfo)
{
	currentInfoRequest = NULL;
	setCursor(Qt::ArrowCursor);

	if (!fault.isNull())
	{
		UiTools::handleError(QApplication::activeWindow(), tr(INFO_GET_ERROR_TITLE).arg(currentContact.getDisplayName()), fault);
		return;
	}

	if(!channelInfo.isNull())
	{
		currentContact = UserInfo(channelInfo.getChannelId(), channelInfo.getTitle(), channelInfo.getTitle(), QDateTime(), channelInfo.getShortDescription(), QString(), QLocale::AnyCountry, QString(), Sex::Undefined, channelInfo.getCreationDate(), channelInfo.getLogoUrl(), UserStatus::Undefined);

		loadPicture();

		updateInfo();
	}
}

void ContactInfoDialog::onRequestCancelled()
{
	if (currentInfoRequest != NULL)
		currentInfoRequest->detach();

	setCursor(Qt::ArrowCursor);
}

void ContactInfoDialog::init(UserInfo &userInfo, bool isSpecialEntry, bool isByLogin)
{
	this->setAttribute(Qt::WA_DeleteOnClose);

	TimeMeasure t("ContactInfoDialog");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	connect(PICTURELOADER, SIGNAL(newPictureLoaded(const QString &)), this, SLOT(onNewPictureLoaded(const QString &)));

	this->isByLogin = isByLogin;
	currentInfoRequest = NULL;
	currentContact = userInfo;

	UiTools::moveWindowToScreenCenter(this);

	contactAvatarToolButton->setThumbnailMode(false);
	contactAvatarToolButton->setContactId(currentContact.getUserId());

	// Hide potentially empty info
	ageLabel->hide();
	countryLabel->hide();
	cityLabel->hide();
	aboutLabel->hide();

	addAsFriendButton->setVisible(false);

	nameLinkButton->setDisabled(isSpecialEntry);

	updateInfo();

	connect(closeButton, SIGNAL(clicked()), this, SLOT(close()));
	connect(closeToolButton, SIGNAL(clicked()), this, SLOT(close()));

	addAsFriendButtonVisible = true;

	// Send request for more info
	if(!isByLogin)
	{
		qint32 userId = currentContact.getUserId();

		if(!isSpecialEntry)
		{
			currentInfoRequest = new ContactInfoRequest(this, userId, type);

			addAsFriendButtonVisible = false;
			setCursor(Qt::BusyCursor);
		}

		if(userId != -1)
		{
			loadPicture();

		}
	}
	else
	{
		currentInfoRequest = new ContactInfoRequest(this, currentContact.getLogin());

		setCursor(Qt::BusyCursor);
	}
}

void ContactInfoDialog::retranslate()
{
	retranslateUi(this);

	titleLabel->setText(tr(INFO_TITLE).arg(currentContact.getLogin()));
	closeButton->setText(tr(CLOSE_TEXT));
	int age = currentContact.getAge();
	( age >= 0 ? ageLabel->setText(tr(AGE_LABEL).arg(age)) : ageLabel->setText(QString()) );
}

void ContactInfoDialog::mousePressEvent(QMouseEvent *event)
{
	if (event->button() == Qt::LeftButton)
	{
		QPoint topLeft = event->globalPos() - frameGeometry().topLeft();
		QPoint bottomRight = frameGeometry().bottomRight() - event->globalPos();
		
		if (topLeft.x() < 0 || topLeft.y() < 0 || 
			bottomRight.x() < 0 || bottomRight.y() < 0)
		{
			this->close();
			return;
		}

		dragPosition = event->globalPos() - frameGeometry().topLeft();

		isMousePressed = true;
		event->accept();
	}
}

void ContactInfoDialog::updateInfo()
{
	retranslate();

	// Display name
	nameLinkButton->setText(currentContact.getDisplayName());

	if (isByLogin)
		nameLinkButton->setVisible(currentContact.getUserId() > 0);

	// Display motto
	mottoLabel->setText(currentContact.getMotto());

	// Display user age
	if (currentContact.getAge() > 0)
	{
		ageLabel->show();
	}

	// Display gender icon
	Sex::SexEnum gender = currentContact.getSex();
	switch (gender)
	{
		case Sex::Male:
			genderIcoLabel->setPixmap(QPixmap(MALE_ICON));
			genderIcoLabel->setToolTip(tr(MALE_TOOLTIP));
			break;
		case Sex::Female:
			genderIcoLabel->setPixmap(QPixmap(FEMALE_ICON));
			genderIcoLabel->setToolTip(tr(FEMALE_TOOLTIP));
			break;
			
		default:
			break;
	}
	
	// Display country
	if (currentContact.getCountry() != QLocale::AnyCountry)
	{
		QLocale::Country country = currentContact.getCountry();
		countryLabel->setText(QString(COUNTRY_LABEL).arg(UiTools::getCountryName(country)));
		countryLabel->show();
	}

	// Display city
	if (!currentContact.getCity().isEmpty())
	{
		cityLabel->setText(currentContact.getCity());
		cityLabel->show();
	}

	// Display about me
	if (!currentContact.getAboutMe().isEmpty())
	{
		aboutLabel->setText(currentContact.getAboutMe());
		aboutLabel->show();
	}
}

void ContactInfoDialog::on_nameLinkButton_clicked()
{
	QUrl profileUrl = QUrl((type == ContactType::Friend ? QString(PROFILE_LINK) : QString(CHANNEL_LINK)).arg(currentContact.getUserId()));
	QDesktopServices::openUrl(profileUrl);
}

void ContactInfoDialog::on_addAsFriendButton_clicked()
{
	hide();
	AuthorizationDialog *authDialog = new AuthorizationDialog(parentWidget());
	authDialog->request(currentContact);
	authDialog->setAttribute(Qt::WA_DeleteOnClose);
	close();
}

void ContactInfoDialog::onNewPictureLoaded(const QString &key)
{
	if (key.toInt() != currentContact.getUserId())
		return;

	QPixmap picture;
	if (PICTURELOADER->find(key, picture))
		contactAvatarToolButton->setPicture(picture);
}

void ContactInfoDialog::loadPicture()
{
	QPixmap picture;
	QString id = QString::number(currentContact.getUserId());
	bool find = PICTURELOADER->find(id, picture, currentContact.getUserpicUrl());

	if (!find)
	{
		if (type == ContactType::Channel)
		{
			picture.load(CHANNEL_DEFAULT_AVATAR);
			find = true;
		}
		if (type == ContactType::Friend)
		{
			picture.load(USERPIC_DEFAULT);
			find = true;
		}
	}

	if (find)
		contactAvatarToolButton->setPicture(picture);
}

}