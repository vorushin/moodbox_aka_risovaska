#include "backgroundrequests.h"

#include "debug.h"
#include "testtools.h"
#include "faulttools.h"

namespace MoodBox
{

// InfoManagerBackgroundRequest class
InfoManagerBackgroundRequest::InfoManagerBackgroundRequest() : RandomRetryTimerObject(INFOMANAGER)
{
	Q_ASSERT_X(INFOMANAGER->getIsLoggedOn(), "InfoManagerBackgroundRequest", "Starting request when not logged in");

	setTimerIntervalLimit(BACKGROUND_REQUEST_RETRY_MAXIMUM_INTERVAL);
	setRandomTimerInterval(BACKGROUND_REQUEST_RETRY_INTERVAL);
	setRandomTimerRange(0, BACKGROUND_REQUEST_RETRY_INTERVAL);
}

InfoManagerBackgroundRequest::~InfoManagerBackgroundRequest()
{
	stopTimer();
}

void InfoManagerBackgroundRequest::repeatRequest()
{
	if (!hasMoreTries())
	{
		completeRequest(false);
	}
	else
	{
		QDEBUG("Repeating failed request with timeout " << getRetryTimerInterval());
		startNewTryTimer();		
	}
}

void InfoManagerBackgroundRequest::processServerResponse(const QVariant &state, const Fault &fault)
{
	this->state = state;
	this->fault = fault;

	if (!fault.isNull())
	{
		if (FaultTools::getFaultReaction(fault) != FaultTools::StopRequests && INFOMANAGER->isUserOnline())
		{
			repeatRequest();
			return;
		}
	}
	else
	{
		completeRequest(true);
	}

	deleteLater();
}

void InfoManagerBackgroundRequest::onTimerTimeout()
{
	sendRequest();
}

// GetMyAccountRequest class
GetMyAccountRequest::GetMyAccountRequest() : InfoManagerBackgroundRequest()
{
	connect(this, SIGNAL(gotMyAccountResult(QVariant, Fault, UserAccount)), INFOMANAGER, SLOT(onGetMyAccountResult(QVariant, Fault, UserAccount)));
	
	askForAccount();
}

void GetMyAccountRequest::sendRequest()
{
	askForAccount();
}

void GetMyAccountRequest::completeRequest(bool succeeded)
{
	emit gotMyAccountResult(state, fault, (succeeded) ? account : UserAccount());
}

void GetMyAccountRequest::askForAccount()
{
	SERVER->getMyAccount(CALLBACK(this, onGetMyAccountResult, UserAccount));
}

void GetMyAccountRequest::onGetMyAccountResult(QVariant state, Fault fault, UserAccount account)
{
	// Start of test code
	/*
	static TestCounter counter(5);
	
	if (counter.needMore())
		fault = Fault("test", "test", "test");
	*/
	// End of test code

	this->account = account;
	processServerResponse(state, fault);
}

// GetContactsRequest class
GetContactsRequest::GetContactsRequest() : InfoManagerBackgroundRequest()
{
	connect(this, SIGNAL(gotContactsResult(QVariant, Fault, QList<ContactInfo>)), INFOMANAGER, SLOT(onGetContactsResult(QVariant, Fault, QList<ContactInfo>)));

	askForContacts();
}

void GetContactsRequest::sendRequest()
{
	askForContacts();
}

void GetContactsRequest::completeRequest(bool succeeded)
{
	emit gotContactsResult(state, fault, (succeeded) ? contacts : QList<ContactInfo>());
}

void GetContactsRequest::askForContacts()
{
	SERVER->getContacts(CALLBACK(this, onGetContactsResult, QList<ContactInfo>));
}

void GetContactsRequest::onGetContactsResult(QVariant state, Fault fault, QList<ContactInfo> result)
{
	// Start of test code
	/*
	static TestCounter counter(5);
	
	if (counter.needMore())
		fault = Fault("TransportError", "test", "test");
	*/
	// End of test code

	this->contacts = result;

	processServerResponse(state, fault);
}

// GetContactRequest class
GetContactRequest::GetContactRequest(qint32 id, bool isAuthorizing) : InfoManagerBackgroundRequest()
{
	this->id = id;
	
	if (isAuthorizing)
		connect(this, SIGNAL(gotContactResult(QVariant, Fault, ContactInfo)), INFOMANAGER, SLOT(onGetAuthorizingContactResult(QVariant, Fault, ContactInfo)));
	else
		connect(this, SIGNAL(gotContactResult(QVariant, Fault, ContactInfo)), INFOMANAGER, SLOT(onGetContactResult(QVariant, Fault, ContactInfo)));

	askForContact();
}

void GetContactRequest::sendRequest()
{
	askForContact();
}

void GetContactRequest::completeRequest(bool succeeded)
{
	emit gotContactResult(state, fault, (succeeded) ? contact : ContactInfo());
}

void GetContactRequest::askForContact()
{
	SERVER->getContact(CALLBACK(this, onGetContactResult, ContactInfo), id, id);
}

void GetContactRequest::onGetContactResult(QVariant state, Fault fault, ContactInfo result)
{
	// Start of test code
	/*
	static TestCounter counter(5);
	
	if (counter.needMore())
		fault = Fault("test", "test", "test");
	*/
	// End of test code

	this->contact = result;

	processServerResponse(state, fault);
}

// GetStatusRequest class
GetStatusRequest::GetStatusRequest(qint32 id) : InfoManagerBackgroundRequest()
{
	this->id = id;

	connect(this, SIGNAL(gotStatusResult(QVariant, Fault, UserStatus::UserStatusEnum)), INFOMANAGER, SLOT(onGetStatusResult(QVariant, Fault, UserStatus::UserStatusEnum)));

	askForStatus();
}

void GetStatusRequest::sendRequest()
{
	askForStatus();
}

void GetStatusRequest::completeRequest(bool succeeded)
{
	emit gotStatusResult(state, fault, (succeeded) ? status : UserStatus::Undefined);
}

void GetStatusRequest::askForStatus()
{
	SERVER->getStatus(CALLBACK(this, onGetStatusResult, UserStatus::UserStatusEnum), id, id);
}

void GetStatusRequest::onGetStatusResult(QVariant state, Fault fault, UserStatus::UserStatusEnum result)
{
	// Start of test code
	/*
	static TestCounter counter(5);
	
	if (counter.needMore())
		fault = Fault("test", "test", "test");
	*/
	// End of test code

	this->status = result;

	processServerResponse(state, fault);
}

// GetAuthorizationRequest class
GetAuthorizationRequest::GetAuthorizationRequest(qint32 id) : InfoManagerBackgroundRequest()
{
	this->id = id;

	connect(this, SIGNAL(gotAuthorizationResult(QVariant, Fault, Authorization)), INFOMANAGER, SLOT(onGetAuthorizationResult(QVariant, Fault, Authorization)));

	askForAuthorization();
}

void GetAuthorizationRequest::sendRequest()
{
	askForAuthorization();
}

void GetAuthorizationRequest::completeRequest(bool succeeded)
{
	emit gotAuthorizationResult(state, fault, (succeeded) ? authorization : Authorization());
}

void GetAuthorizationRequest::askForAuthorization()
{
	SERVER->getAuthorization(CALLBACK(this, onGetAuthorizationResult, Authorization), id, id);
}

void GetAuthorizationRequest::onGetAuthorizationResult(QVariant state, Fault fault, Authorization result)
{
	// Start of test code
	/*
	static TestCounter counter(5);
	
	if (counter.needMore())
		fault = Fault("test", "test", "test");
	*/
	// End of test code

	this->authorization = result;

	processServerResponse(state, fault);
}

}