#include "channellistitem.h"

#include "picturefileloader.h"
#include "peopleinfomanager.h"
#include "contactinfo.h"
#include "formblocker.h"

#include "uitools.h"
#include "international.h"
#include "removecontactdialog.h"
#include "testtools.h"

namespace MoodBox
{

// AddChannelRequest class
AddChannelRequest::AddChannelRequest(ChannelListItem *parent, const ChannelResult &channelInfo)
	: ServerRequest()
{
	this->channelInfo = channelInfo;

	connect(this, SIGNAL(addChannelRequestCompleted(Fault, ChangeUserChannelResult::ChangeUserChannelResultEnum)), parent, SLOT(onGetAddChannelResult(Fault, ChangeUserChannelResult::ChangeUserChannelResultEnum)));
	
	SERVER->addUserToChannel(CALLBACK(this, onGetAddChannelRequestResult, ChangeUserChannelResult::ChangeUserChannelResultEnum), channelInfo.getChannelId());
}

void AddChannelRequest::onGetAddChannelRequestResult(QVariant state, Fault fault, ChangeUserChannelResult::ChangeUserChannelResultEnum result)
{
	Q_UNUSED(state)
	
	if (fault.isNull())
	{
		if (result == ChangeUserChannelResult::Ok && INFOMANAGER->isUserOnline())
		{
			ContactInfo tmpInfo(channelInfo.getChannelId(), channelInfo.getTitle(), UserStatus::Online, channelInfo.getShortDescription(), channelInfo.getTitle(), channelInfo.getCreationDate(), AuthorizationState::Authorized, QString(), false, ContactType::Channel);
			INFOMANAGER->addToContactList(tmpInfo);
		}
	}

	if (active)
		emit addChannelRequestCompleted(fault, result);

	deleteLater();
}

// RemoveChannelRequest class
RemoveChannelRequest::RemoveChannelRequest(ChannelListItem *parent, const qint32 &channelId)
	: ServerRequest()
{
	this->channelId = channelId;

	connect(this, SIGNAL(removeChannelRequestCompleted(Fault, ChangeUserChannelResult::ChangeUserChannelResultEnum)), parent, SLOT(onGetRemoveChannelResult(Fault, ChangeUserChannelResult::ChangeUserChannelResultEnum)));

	SERVER->deleteUserFromChannel(CALLBACK(this, onGetRemoveChannelRequestResult, ChangeUserChannelResult::ChangeUserChannelResultEnum), channelId, channelId);
}

void RemoveChannelRequest::onGetRemoveChannelRequestResult(QVariant state, Fault fault, ChangeUserChannelResult::ChangeUserChannelResultEnum result)
{
	Q_UNUSED(state)
	
	if (fault.isNull())
	{
		if (result == ChangeUserChannelResult::Ok && INFOMANAGER->isUserOnline())
			INFOMANAGER->removeFromContactList(channelId);
	}

	if (active)
		emit removeChannelRequestCompleted(fault, result);

	deleteLater();
}

// ChannelListItem class
ChannelListItem::ChannelListItem(QWidget *parent)
	: ServerFrame(parent), isKnownChannel(false), currentAddRequest(NULL), currentRemoveRequest(NULL)
{
	TimeMeasure t("ChannelListItem");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	connect(PICTURELOADER, SIGNAL(newPictureLoaded(const QString &)), this, SLOT(onNewPictureLoaded(const QString &)));

	addChannelButton->hide();
	removeChannelButton->hide();

	formBlocker->addWidget(addChannelButton);
	formBlocker->addWidget(removeChannelButton);
}

ChannelListItem::~ChannelListItem()
{
	if (currentAddRequest != NULL)
		currentAddRequest->detach();
	
	if (currentRemoveRequest != NULL)
		currentRemoveRequest->detach();
}

void ChannelListItem::setChannelInfo(const ChannelResult &channelInfo)
{
	this->channelInfo = channelInfo;
	channelKey = QString::number(channelInfo.getChannelId());

	// Buttons
	updateSubscriptionStatus();

	// Text
	titleLabel->setText(channelInfo.getTitle());
	descriptionLabel->setText(channelInfo.getShortDescription());

	// Picture
	QPixmap picture;
	if (!PICTURELOADER->find(channelKey, picture, channelInfo.getLogoUrl()))
		picture.load(CHANNEL_DEFAULT_AVATAR);

	imageLabel->setPixmap(picture);
}

void ChannelListItem::updateSubscriptionStatus()
{
	isKnownChannel = INFOMANAGER->isKnownPerson(channelInfo.getChannelId());

	addChannelButton->setVisible(!isKnownChannel);
	removeChannelButton->setVisible(isKnownChannel);

	setStyleSheet( (isKnownChannel) ? "#ChannelListItemClass{background-color: rgb(255, 255, 255); padding-bottom: -1px; border: none; border-bottom: 1px solid rgb(189, 189, 189);}" 
									: "#ChannelListItemClass{background-color: rgb(240, 240, 240); padding-bottom: -1px; border: none; border-bottom: 1px solid rgb(189, 189, 189);}");
}

void ChannelListItem::onGetAddChannelResult(Fault fault, ChangeUserChannelResult::ChangeUserChannelResultEnum result)
{
	currentAddRequest = NULL;
	formBlocker->unblock();

	if (!fault.isNull())
	{
		UiTools::handleError(this, tr(ADD_CHANNEL_ERROR_TITLE).arg(channelInfo.getTitle()), fault);
		return;
	}
		
	if (result != ChangeUserChannelResult::Ok)
	{
		QString error = PromptHelper::getChangeUserChannelErrorName(result).arg(channelInfo.getTitle());

		UiTools::handleError(QApplication::activeWindow(), tr(ADD_CHANNEL_ERROR_TITLE).arg(channelInfo.getTitle()), error);
	}
}

void ChannelListItem::onGetRemoveChannelResult(Fault fault, ChangeUserChannelResult::ChangeUserChannelResultEnum result)
{
	currentRemoveRequest = NULL;
	formBlocker->unblock();

	if (!fault.isNull())
	{
		UiTools::handleError(this, tr(REMOVE_ERROR_TITLE).arg(channelInfo.getTitle()), fault);
		return;
	}
		
	if (result != ChangeUserChannelResult::Ok)
	{
		QString error = PromptHelper::getChangeUserChannelErrorName(result).arg(channelInfo.getTitle());

		UiTools::handleError(QApplication::activeWindow(), tr(REMOVE_ERROR_TITLE).arg(channelInfo.getTitle()), error);
	}
}

void ChannelListItem::onRequestCancelled()
{
	if (currentAddRequest != NULL)
		currentAddRequest->detach();

	if (currentRemoveRequest != NULL)
		currentRemoveRequest->detach();

	formBlocker->unblock();
}

void ChannelListItem::onNewPictureLoaded(const QString &key)
{
	if (key != channelKey)
		return;

	QPixmap newPixmap;
	if (PICTURELOADER->find(channelKey, newPixmap))
		imageLabel->setPixmap(newPixmap);
}

void ChannelListItem::on_addChannelButton_clicked()
{
	formBlocker->block(true);

	currentAddRequest = new AddChannelRequest(this, channelInfo);
}

void ChannelListItem::on_removeChannelButton_clicked()
{
	formBlocker->block(true);

	currentRemoveRequest = new RemoveChannelRequest(this, channelInfo.getChannelId());
}

}