#ifndef MOODBOXNOTIFICATIONSERVER_H
#define MOODBOXNOTIFICATIONSERVER_H

#include <QObject>
#include <QTimer>

#include "authticketresult.h"
#include "moodboxserver.h"
#include "notificationregistrationresult.h"
#include "notificationresult.h"
#include "okresultcode.h"

// Number of seconds to add to base timeout
#define NOTIFICATION_TIMEOUT_EXTRA 10
// Default number of seconds for notification timeout
#define DEFAULT_NOTIFICATION_TIMEOUT 300
// Number of seconds to try for next connection on error
#define NOTIFICATION_RETRY_TIME 30

namespace MoodBox
{

/*
Provides notification server functionality---waits for notifications and signals about them further.
*/
class MoodBoxNotificationServer : public QObject, public MoodBoxServer
{
	Q_OBJECT

public:
	MoodBoxNotificationServer(Model* model, TransportChannelBase* channel, QString notificationKey, AuthTicketResult authTicket);
	void setAuthTicket(AuthTicketResult authTicket);

	void logout();
	void runNotificationsLoop();
	void init();

signals:
	void notificationResult(Fault fault, QList<Notification> notifications);
	void disconnectEvent();
	void notificationUnregisterCompleted();

public slots:
	// Connects to the notification server and wait for notifications
	void retrieveNextNotifications();

protected:
	QString notificationKey;
	qint64 lastNotificationId;
	AuthTicketResult authTicket;
	// Special flag to detect we're in logout state
	bool isLogout;

protected:
	virtual Header getHeader();

protected slots:
	void onGetNotifications(QVariant state,Fault fault, NotificationResult result);
	void onGetNotificationTimeoutValue(QVariant state, Fault fault, qint32 result);
	void doNotificationUnregister();
	void onNotificationUnregister(QVariant state, Fault fault, OkResultCode::OkResultCodeEnum result);
};

}

#endif // MOODBOXNOTIFICATIONSERVER_H
