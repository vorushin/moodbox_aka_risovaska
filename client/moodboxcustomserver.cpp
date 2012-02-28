#include "moodboxcustomserver.h"

#include <QByteArray>

#include "debug.h"
#include "httpchannel2.h"
#include "logondataprovider.h"
#include "model.h"
#include "moodboxnotificationserver.h"
#include "serverproxysingleton.h"
#include "transportchannelbase.h"
#include "version.h"
#include "language.h"

namespace MoodBox
{

// POSTPONED: do a complete review of this code
MoodBoxCustomServer::MoodBoxCustomServer(Model* model, TransportChannelBase* channel)
	: MoodBoxServer(model, channel),
	header(QByteArray(), VersionTag::DesktopClient, APP_VERSION.toString(), Language::Undefined), 
	notificationServer(NULL), authTicketUpdater(this), maxRequestSize(DEFAULT_REQUEST_SIZE_LIMIT)
{
	channel->setParent(this);

#ifdef RUSSIAN_VERSION
	header.setLanguage(Language::Ru);
#endif

}

Model* MoodBoxCustomServer::getModel()
{
	return model;
}

void MoodBoxCustomServer::logon(Callback callback, QVariant state, QString login, QString password)
{
	// clear old ticket
	header.setAuthTicket(QByteArray());

	AuthTicketResultCallbackDelegate *callbackDelegate = new AuthTicketResultCallbackDelegate(this, callback);
	getAuthTicket(Callback(callbackDelegate, SLOT(onGetAuthTicketResult(QVariant, Fault, AuthTicketResult))), 
		state, login, password);
}

void MoodBoxCustomServer::logon(Callback callback, QString login, QString password)
{
	this->logon(callback, QVariant(), login, password);
}

void MoodBoxCustomServer::logout()
{
	channel->cancelAll();

	if (notificationServer)
	{
		notificationServer->logout();
		notificationServer = NULL;
	}
	setAuthTicketResult(AuthTicketResult());
}

void MoodBoxCustomServer::triggerServerError(ServerError code)
{
	if (code == NotAuthenticated)
	{
		QDEBUG("MoodBoxCustomServer::triggerServerError: authorization failed");
		// Clean authorization ticket
		// Commented this code because ticket anyway will cleared in logout() function, see above
		// setAuthTicketResult(AuthTicketResult());
	}
	emit serverError(code);
}

void MoodBoxCustomServer::resultFaultCall(Callback callback, QVariant state, Fault fault, qint32 resultTypeId)
{
	MoodBoxServer::resultFaultCall(callback, state, fault, resultTypeId);

	if (fault.getCode() == FAULT_NOT_AUTHENTICATED)
	{
		triggerServerError(NotAuthenticated);
	}
	else if (fault.getCode() == FAULT_UNSUPPORTED_CLIENT_VERSION)
	{
		emit triggerServerError(UnsupportedClientVersion);
	}
}

int MoodBoxCustomServer::getUserId() const
{
	Q_ASSERT_X(!authTicketResult.isNull(), "MoodBoxCustomServer::getUserId", "not authenticated");

	if(authTicketResult.isNull())
		return 0;

	return authTicketResult.getUserId();
}

int MoodBoxCustomServer::getMaxRequestSize() const
{
	return maxRequestSize;
}

void MoodBoxCustomServer::setAuthTicketResult(AuthTicketResult result)
{
	// POSTPONED: make this method simpler
	authTicketResult = result;
	QByteArray authTicket;
	if (authTicketResult.isNull())
	{
		header.setAuthTicket(QByteArray());
	}
	else
	{
		header.setAuthTicket(authTicketResult.getAuthTicket());
	}

	updateNotificationServerTicket();
	if (authTicketResult.isNull())
	{
		authTicketUpdater.stop();
	}
	else
	{
		authTicketUpdater.start(authTicketResult.getLifetime());
	}
}

Header MoodBoxCustomServer::getHeader()
{
	return header;
}

void MoodBoxCustomServer::setupNotificationServer(NotificationRegistrationResult notificationData)
{
	if (notificationServer != NULL)
	{
		notificationServer->deleteLater();
	}

	notificationServer = new MoodBoxNotificationServer(model, new HttpChannel2(notificationData.getServer()),
		notificationData.getKey(), authTicketResult);
	notificationServer->setParent(this);
	connect(notificationServer, SIGNAL(notificationResult(Fault, QList<Notification>)), 
		this, SLOT(onNotificationResult(Fault, QList<Notification>)));
	connect(notificationServer, SIGNAL(disconnectEvent()), this, SLOT(onDisconnect()));
	connect(notificationServer, SIGNAL(notificationUnregisterCompleted()), this, SIGNAL(logoutCompleted()));

	notificationServer->init();
}

bool MoodBoxCustomServer::isRegisteredNotification()
{
	return notificationServer != NULL;
}

void MoodBoxCustomServer::updateNotificationServerTicket()
{
	if (notificationServer)
		notificationServer->setAuthTicket(authTicketResult);
}

QVariant MoodBoxCustomServer::storeCallbackData(const Callback &callback, const QVariant &state)
{
	QList<QVariant> callbackData;
	callbackData << QVariant::fromValue(callback) << state;
	return QVariant(callbackData);
}

void MoodBoxCustomServer::restoreCallbackData(const QVariant &storedData, Callback callback, QVariant state)
{
	QList<QVariant> stateData = storedData.toList();
	Q_ASSERT_X(stateData.count() == 2, "MoodBoxCustomServer::restoreCallbackData", "Callback data list must contain exactly 2 elements");
	callback = stateData.takeFirst().value<Callback>();
	state = stateData.takeFirst();
}

void MoodBoxCustomServer::onNotificationResult(Fault fault, QList<Notification> result)
{
	// POSTPONED: don't do logout on fault, only relogin
	if (!fault.isNull())
		triggerServerError(NotAuthenticated);
	else
		emit notificationResult(result);
}

void MoodBoxCustomServer::onDisconnect()
{
	triggerServerError(Disconnect);
}

void MoodBoxCustomServer::onGetServerInfo(QVariant state, Fault fault, ServerInfo info)
{
	Q_UNUSED(state)
	
	if (!fault.isNull() || !info.getMaxRequestSize())
	{
		QDEBUG("MoodBoxCustomServer::onGetServerInfo: unable to get max request size, will use default = " << DEFAULT_REQUEST_SIZE_LIMIT);
		return;
	}

	maxRequestSize = info.getMaxRequestSize();

	channel->setRequestSizeLimit(maxRequestSize);
}

AuthTicketResultCallbackDelegate::AuthTicketResultCallbackDelegate(MoodBoxCustomServer *parent, Callback callback) 
	: QObject(), parent(parent), callback(callback)
{
	// POSTPONED : use [re]storeCallbackData instead of this whole class 
	originalResult = AuthTicketResult::empty();
}

void AuthTicketResultCallbackDelegate::onGetAuthTicketResult(QVariant state, Fault fault, AuthTicketResult result)
{
	connect(this, SIGNAL(logonResult(QVariant, Fault, AuthTicketResult)), callback.target, callback.method);
	bool isLoggedOn = false;

	if (fault.isNull() && !result.getAuthTicket().isEmpty())
	{
		parent->setAuthTicketResult(result);
		if (!parent->isRegisteredNotification())
		{
			originalResult = result;
			parent->notificationRegister(Callback(this, SLOT(onNotificationRegister(QVariant, Fault, NotificationRegistrationResult))), 
				state);
			isLoggedOn = true;
		}
	}

	if (!isLoggedOn)
	{
		emit logonResult(state, fault, result);
		disconnect();
		deleteLater();
	}
}

void AuthTicketResultCallbackDelegate::onNotificationRegister(QVariant state, Fault fault, NotificationRegistrationResult result)
{
	if (!fault.isNull())
	{
		emit logonResult(state, fault, originalResult);
	}
	else
	{
		parent->setupNotificationServer(result);
		emit logonResult(state, fault, originalResult);
		parent->getServerInfo(CALLBACK(parent, onGetServerInfo, ServerInfo));
	}
	disconnect();
	deleteLater();
}

}