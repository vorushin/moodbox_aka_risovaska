#ifndef CHANNELLISTITEM_H
#define CHANNELLISTITEM_H

#include "servercontrol.h"

#include "ui_channellistitem.h"

#include "channelresult.h"
#include "serverrequest.h"
#include "changeuserchannelresult.h"
#include "channelresult.h"

namespace MoodBox
{

#define ADD_CHANNEL_ERROR_TITLE		QT_TRANSLATE_NOOP("MoodBox::ChannelListItem", "AddChannelErrorTitle%1")
#define	ADD_CHANNEL_LABEL_TEXT		QT_TRANSLATE_NOOP("MoodBox::ChannelListItem", "AddChannelLabel%1")

using namespace Ui;

class ChannelListItem;

// Add channel request
class AddChannelRequest : public ServerRequest
{
	Q_OBJECT

public:
	AddChannelRequest(ChannelListItem *parent, const ChannelResult &channelInfo);

signals:
	void addChannelRequestCompleted(Fault fault, ChangeUserChannelResult::ChangeUserChannelResultEnum result);

private:
	ChannelResult channelInfo;

private slots:
	void onGetAddChannelRequestResult(QVariant state, Fault fault, ChangeUserChannelResult::ChangeUserChannelResultEnum result);
};

// Remove channel request
class RemoveChannelRequest : public ServerRequest
{
	Q_OBJECT

public:
	RemoveChannelRequest(ChannelListItem *parent, const qint32 &channelId);

signals:
	void removeChannelRequestCompleted(Fault fault, ChangeUserChannelResult::ChangeUserChannelResultEnum result);

private:
	qint32 channelId;

private slots:
	void onGetRemoveChannelRequestResult(QVariant state, Fault fault, ChangeUserChannelResult::ChangeUserChannelResultEnum result);
};

// Channel list item
class ChannelListItem : public ServerFrame, public ChannelListItemClass
{
	Q_OBJECT

public:
	ChannelListItem(QWidget *parent = 0);
	virtual ~ChannelListItem();

	void setChannelInfo(const ChannelResult &channelInfo);
	void updateSubscriptionStatus();

public slots:
	void onGetAddChannelResult(Fault fault, ChangeUserChannelResult::ChangeUserChannelResultEnum result);
	void onGetRemoveChannelResult(Fault fault, ChangeUserChannelResult::ChangeUserChannelResultEnum result);

	virtual void onRequestCancelled();

private:
	ChannelResult channelInfo;
	bool isKnownChannel;
	QString channelKey;

	AddChannelRequest *currentAddRequest;
	RemoveChannelRequest *currentRemoveRequest;

private slots:
	void onNewPictureLoaded(const QString &key);

	void on_addChannelButton_clicked();
	void on_removeChannelButton_clicked();
};

}

#endif // CHANNELLISTITEM_H
