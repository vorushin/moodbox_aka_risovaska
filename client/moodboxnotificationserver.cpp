#include "moodboxnotificationserver.h"

#include "version.h"
#include "debug.h"
#include "header.h"
#include "serverproxysingleton.h"
#include "transportchannelbase.h"
#include "language.h"

namespace MoodBox
{

MoodBoxNotificationServer::MoodBoxNotificationServer(Model* model, TransportChannelBase* channel, 
													 QString notificationKey, AuthTicketResult authTicket)
	: MoodBoxServer(model, channel), notificationKey(notificationKey), lastNotificationId(0), isLogout(false)
{
	this->channel->setParent(this);
	setAuthTicket(authTicket);
}

void MoodBoxNotificationServer::setAuthTicket(AuthTicketResult authTicket)
{
	this->authTicket = authTicket;
}

void MoodBoxNotificationServer::logout()
{
	if (isLogout) return; // Protection against several logout calls

	isLogout = true;
	channel->cancelAll();
	QDEBUG("MoodBoxNotificationServer::logout: notificationUnregister");
	doNotificationUnregister();
}

void MoodBoxNotificationServer::runNotificationsLoop()
{
	QTimer::singleShot(0, this, SLOT(retrieveNextNotifications()));
}

void MoodBoxNotificationServer::init()
{
	getNotificationTimeout(CALLBACK(this, onGetNotificationTimeoutValue, qint32));
}

void MoodBoxNotificationServer::retrieveNextNotifications()
{
	getNotifications(CALLBACK(this, onGetNotifications, NotificationResult), notificationKey, lastNotificationId);
}

Header MoodBoxNotificationServer::getHeader()
{
	if (authTicket.isNull())
		return MoodBoxServer::getHeader();
	QByteArray ticket = authTicket.getAuthTicket();
	if (ticket.isEmpty())
		return MoodBoxServer::getHeader();
#ifndef RUSSIAN_VERSION
	return Header(ticket, VersionTag::DesktopClient, APP_VERSION.toString(), Language::Undefined);
#else
	return Header(ticket, VersionTag::DesktopClient, APP_VERSION.toString(), Language::Ru);
#endif
}

void MoodBoxNotificationServer::onGetNotifications(QVariant state, Fault fault, NotificationResult result)
{
	Q_UNUSED(state)
	
	// Special case for logging out state
	if (isLogout)
	{
		return;
	}

	if (authTicket.isNull())
	{
		QDEBUG("MoodBoxNotificationServer::onGetNotifications: ticket is empty, exiting notification server...");
		return;
	}

	QList<Notification> notifications;
	if (fault.isNull())
	{
		lastNotificationId = result.getPacketId();
		notifications = result.getNotifications();
	}
	if (!fault.isNull() || !notifications.isEmpty())
	{
		emit notificationResult(fault, notifications);
	}

	if (!notifications.isEmpty())
	{
		// Check whether we received disconnect event
		foreach (Notification item, notifications)
		{
			if (item.getEvent() == Event::Disconnect)
			{
				emit disconnectEvent();
				QDEBUG("MoodBoxNotificationServer::onGetNotifications: received disconnect event from server, stop listening to notifications");
				return;
			}
		}
	}

	if (fault.isNull())
		runNotificationsLoop();
}

void MoodBoxNotificationServer::onGetNotificationTimeoutValue(QVariant state, Fault fault, qint32 result)
{
	Q_UNUSED(state)
	
	if (!fault.isNull())
	{
		QDEBUG("MoodBoxNotificationServer::onGetNotificationTimeoutValue: unable to get notification timeout, using default = " << DEFAULT_NOTIFICATION_TIMEOUT);
		result = DEFAULT_NOTIFICATION_TIMEOUT;
	}
	channel->setRequestTimeout(result + NOTIFICATION_TIMEOUT_EXTRA);
	runNotificationsLoop();
}

void MoodBoxNotificationServer::doNotificationUnregister()
{
	notificationUnregister(CALLBACK(this, onNotificationUnregister, OkResultCode::OkResultCodeEnum), notificationKey);
}

void MoodBoxNotificationServer::onNotificationUnregister(QVariant state, Fault fault, OkResultCode::OkResultCodeEnum result)
{
	Q_UNUSED(state);
	Q_UNUSED(fault);
	Q_UNUSED(result);
#ifndef QT_NO_DEBUG
	if (!fault.isNull() || result != OkResultCode::Ok)
		QDEBUG("MoodBoxNotificationServer::onNotificationUnregister: notification unregister failed");
#endif
	emit notificationUnregisterCompleted();
	disconnect();
	deleteLater();
}

}