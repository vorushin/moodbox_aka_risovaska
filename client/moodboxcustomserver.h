#ifndef MOODBOXCUSTOMSERVER_H
#define MOODBOXCUSTOMSERVER_H

#include <QObject>
#include <QTimer>

#include "authticketupdater.h"
#include "authticketresult.h"
#include "moodboxserver.h"
#include "notification.h"
#include "notificationregistrationresult.h"
#include "serverinfo.h"
#include "header.h"

#include "moodboxfaultcodes.h"

namespace MoodBox
{

class HttpChannel;
class Model;
class MoodBoxNotificationServer;
class TransportChannelBase;

/*
Descendor of MoodBoxServer which is used for all server-side methods.
We're using this class to override some methods or provide extended functionality, see logon() for example.
*/
class MoodBoxCustomServer : public QObject, public MoodBoxServer
{
	Q_OBJECT

friend class AuthTicketResultCallbackDelegate; // Necessary for logon() method
public:
	MoodBoxCustomServer(Model* model, TransportChannelBase* channel);

	Model* getModel();

	// Performs a client logon, signalled via logonResult
	void logon(Callback callback, QVariant state, QString login, QString password);
	void logon(Callback callback, QString login, QString password);
	// Client logout
	void logout();
	// Triggers serverError signal from some other place
	virtual void triggerServerError(ServerError code);
	virtual void resultFaultCall(Callback callback, QVariant state, Fault fault, qint32 resultTypeId);

	int getUserId() const;
	int getMaxRequestSize() const;

signals:
	void serverError(ServerResponseHandler::ServerError code);
	// Triggered on new notification result
	void notificationResult(QList<Notification> result);
	void logoutCompleted();

protected:
	Header header;
	MoodBoxNotificationServer *notificationServer;
	AuthTicketResult authTicketResult;
	AuthTicketUpdater authTicketUpdater;
	int maxRequestSize;

protected:
	void setAuthTicketResult(AuthTicketResult result);
	virtual Header getHeader();
	// Creates notification server instance
	void setupNotificationServer(NotificationRegistrationResult notificationData);
	// Returns whether we registered on notification server
	bool isRegisteredNotification();

	void updateNotificationServerTicket();
	// Wraps callback and state in QVariant
	QVariant storeCallbackData(const Callback &callback, const QVariant &state);
	void restoreCallbackData(const QVariant &storedData, Callback callback, QVariant state);

protected slots:
	// Method to receive notifications from child notification server
	void onNotificationResult(Fault fault, QList<Notification> result);
	void onDisconnect();
	// Callback for getting max request size call
	void onGetServerInfo(QVariant state, Fault fault, ServerInfo info);
};

// Used to pass signal to real callback, must be created via "new" only!
class AuthTicketResultCallbackDelegate : public QObject
{
	Q_OBJECT

friend class MoodBoxCustomServer; // To avoid dangerous usage from innocent classes
signals:
	void logonResult(QVariant state, Fault fault, AuthTicketResult result);

protected:
	MoodBoxCustomServer *parent;
	Callback callback;
	AuthTicketResult originalResult;

protected slots:
	void onGetAuthTicketResult(QVariant state, Fault fault, AuthTicketResult result);
	void onNotificationRegister(QVariant state, Fault fault, NotificationRegistrationResult result);

private:
	AuthTicketResultCallbackDelegate(MoodBoxCustomServer *parent, Callback callback);
};

}

#endif // MOODBOXCUSTOMSERVER_H
