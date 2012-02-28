#include "findchannelframe.h"

#include "serverproxysingleton.h"
#include "channellistitem.h"
#include "peopleinfomanager.h"

#include "testtools.h"

namespace MoodBox
{

SearchChannelRequest::SearchChannelRequest(FindChannelFrame *parent, qint32 pageNumber, qint32 recordsPerPage, QString value)
 : ServerRequest()
{
	connect(this, SIGNAL(searchCompleted(Fault, ChannelSearchResult)), parent, SLOT(onSearchContacts(Fault, ChannelSearchResult)));

	SERVER->searchChannel(CALLBACK(this, onSearchResult, ChannelSearchResult), pageNumber, recordsPerPage, value);
}

void SearchChannelRequest::onSearchResult(QVariant state, Fault fault, ChannelSearchResult result)
{
	Q_UNUSED(state)
	
	if (active)
		emit searchCompleted(fault, result);

	deleteLater();
}

FindChannelFrame::FindChannelFrame(QWidget *parent)
	: ServerFrame(parent), currentRequest(NULL), spacer(NULL)
{
	TimeMeasure t("FindChannelFrame");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	listHost = new QWidget(this);
	listHost->setLayout(new QVBoxLayout(listHost));
	listHost->layout()->setMargin(CHANNEL_LIST_INTERNAL_SPACING);
	listHost->layout()->setSpacing(CHANNEL_LIST_INTERNAL_SPACING);

	formBlocker->addWidget(channelList);

	connect(INFOMANAGER, SIGNAL(contactListChanged()), this, SLOT(onContactListChanged()));
}

void FindChannelFrame::queryChannels()
{
	formBlocker->block();
	currentRequest = new SearchChannelRequest(this, 1, CHANNELS_PER_PAGE, QString());
}

void FindChannelFrame::onSearchContacts(Fault fault, ChannelSearchResult result)
{
	currentRequest = NULL;
	formBlocker->unblock();

	if (!fault.isNull())
	{
		UiTools::handleError(this, tr(SEARCH_CHANNELS_ERROR_TITLE), fault);
		return;
	}

	populateChannelList(result.getItems());
}

void FindChannelFrame::onRequestCancelled()
{
	if (currentRequest != NULL)
		currentRequest->detach();

	formBlocker->unblock();
}

void FindChannelFrame::populateChannelList(const QList <ChannelResult> &searchResult)
{
	setCursor(Qt::WaitCursor);

	clearChannelList();
	
	foreach(ChannelResult channelResult, searchResult)
	{
		ChannelListItem *item = new ChannelListItem(listHost);

		listHost->layout()->addWidget(item);

		item->setChannelInfo(channelResult);
		item->setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);

		channels.append(item);
	}

	spacer = new QSpacerItem(1, 1, QSizePolicy::Minimum, QSizePolicy::Expanding);
	listHost->layout()->addItem(spacer);

	channelList->setWidget(listHost);

	setCursor(Qt::ArrowCursor);
}

void FindChannelFrame::clearChannelList()
{
	foreach(ChannelListItem *channel, channels)
		channel->deleteLater();

	channels.clear();

	listHost->layout()->removeItem(spacer);

	if (spacer != NULL)
	{
		delete spacer;
		spacer = NULL;
	}

	channelList->setWidget(NULL);
}

void FindChannelFrame::onContactListChanged()
{
	if (!isVisible())
		return;

	foreach(ChannelListItem *channel, channels)
		channel->updateSubscriptionStatus();
}

}