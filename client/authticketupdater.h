#ifndef AUTHTICKETUPDATER_H
#define AUTHTICKETUPDATER_H

#include <QObject>
#include <QTimer>

#include "authticketresult.h"
#include "fault.h"

namespace MoodBox
{

// Number of seconds to update auth ticket before its expiration
#define AUTH_TICKET_UPDATE_GAP	180
// Number of retries to update authorization ticket automatically
#define MAX_AUTH_UPDATE_RETRIES	3
// Time between ticket update retries
#define AUTH_UPDATE_RETRY_INTERVAL	30

// Automatically updates authorization ticket
class AuthTicketUpdater : public QObject
{
	Q_OBJECT

public:
	AuthTicketUpdater(QObject *parent = 0);

	// Handles update process state
	void start(qint32 ticketLifeTime);
	void stop();

protected:
	QTimer authUpdateTimer, updateRetryTimer;
	int currentRetry;

protected slots:
	// Time to update authorization ticket
	void onAuthUpdateTimer();
	// Auto-retry to update authorization ticket
	void onAuthUpdateRetry(QVariant state, Fault fault, AuthTicketResult result);
};

}

#endif // AUTHTICKETUPDATER_H
