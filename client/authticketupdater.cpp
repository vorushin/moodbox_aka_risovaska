#include "authticketupdater.h"

#include "debug.h"
#include "logondataprovider.h"
#include "serverproxysingleton.h"

namespace MoodBox
{

AuthTicketUpdater::AuthTicketUpdater(QObject *parent) : QObject(parent), currentRetry(0)
{
	authUpdateTimer.setSingleShot(true);
	connect(&authUpdateTimer, SIGNAL(timeout()), this, SLOT(onAuthUpdateTimer()));
	updateRetryTimer.setSingleShot(true);
	connect(&updateRetryTimer, SIGNAL(timeout()), this, SLOT(onAuthUpdateTimer()));
}

void AuthTicketUpdater::start(qint32 ticketLifeTime)
{
	stop();

	int updateInterval = ticketLifeTime - AUTH_TICKET_UPDATE_GAP;

	if (updateInterval < AUTH_TICKET_UPDATE_GAP)
	{
		QDEBUG("AuthTicketUpdater::start: ticket lifetime is too small, only" << ticketLifeTime << "seconds");
		updateInterval = AUTH_TICKET_UPDATE_GAP;
	}

	authUpdateTimer.start(updateInterval * 1000);
	QDEBUG("AuthTicketUpdater::start: will update ticket in" << updateInterval << "seconds");
}

void AuthTicketUpdater::stop()
{
	authUpdateTimer.stop();
	updateRetryTimer.stop();
	currentRetry = 0;
}

void AuthTicketUpdater::onAuthUpdateTimer()
{
	SERVER->logon(CALLBACK(this, onAuthUpdateRetry, AuthTicketResult), LOGONPROVIDER->getCurrentLogin(), LOGONPROVIDER->getCurrentPassword());
}

void AuthTicketUpdater::onAuthUpdateRetry(QVariant state, Fault fault, AuthTicketResult result)
{
	Q_UNUSED(state)
	Q_UNUSED(result)
	
	if (fault.isNull())
	{
		QDEBUG("AuthTicketUpdater::onAuthUpdateRetry: auth ticket updated ok");
		return;
	}
	if (currentRetry >= MAX_AUTH_UPDATE_RETRIES)
	{
		QDEBUG("AuthTicketUpdater::onAuthUpdateRetry: unable to update ticket in" << MAX_AUTH_UPDATE_RETRIES << "retries");
		return;
	}

	currentRetry++;

	QDEBUG("AuthTicketUpdater::onAuthUpdateRetry: retry #" << currentRetry << "in"
		<< AUTH_UPDATE_RETRY_INTERVAL << "seconds, previous fault =" << fault.getDescription());
	updateRetryTimer.start(AUTH_UPDATE_RETRY_INTERVAL * 1000);
}

}