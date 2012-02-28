#ifndef FINDCHANNELFRAME_H
#define FINDCHANNELFRAME_H

#include "servercontrol.h"

#include <QList>

#include "ui_findchannelframe.h"

#include "serverrequest.h"
#include "channelsearchresult.h"

class QSpacerItem;

namespace MoodBox
{

#define SEARCH_CHANNELS_ERROR_TITLE			QT_TRANSLATE_NOOP("MoodBox::FindChannelFrame", "SearchChannelsErrorTitle")
#define SEARCH_CHANNELS_PROGRESS_TEXT		QT_TRANSLATE_NOOP("MoodBox::FindChannelFrame", "SearchChannelsProgressText")

#define CHANNEL_LIST_INTERNAL_SPACING		0
#define CHANNELS_PER_PAGE					50

using namespace Ui;

class FindChannelFrame;
class ChannelListItem;

// Channel search request
class SearchChannelRequest : public ServerRequest
{
	Q_OBJECT

public:
	SearchChannelRequest(FindChannelFrame *parent, qint32 pageNumber, qint32 recordsPerPage, QString value);
	
signals:
	void searchCompleted(Fault fault, ChannelSearchResult result);

private slots:
	void onSearchResult(QVariant state, Fault fault, ChannelSearchResult result);
};

// Find channels for subscription
class FindChannelFrame : public ServerFrame, public FindChannelFrameClass
{
	Q_OBJECT

public:
	FindChannelFrame(QWidget *parent = 0);

	void queryChannels();

public slots:
	void onSearchContacts(Fault fault, ChannelSearchResult result);

	virtual void onRequestCancelled();

private:
	SearchChannelRequest *currentRequest;

	QList <ChannelListItem *> channels;

	QWidget *listHost;
	QSpacerItem *spacer;

	void populateChannelList(const QList <ChannelResult> &searchResult);
	void clearChannelList();

private slots:
	void onContactListChanged();
};

}

#endif // FINDCHANNELFRAME_H
