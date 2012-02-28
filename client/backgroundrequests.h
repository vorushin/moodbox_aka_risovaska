#ifndef BACKGROUNDREQUESTS_H
#define BACKGROUNDREQUESTS_H

#include "timerobjects.h"
#include "peopleinfomanager.h"

namespace MoodBox
{

#define BACKGROUND_REQUEST_RETRY_MAXIMUM_INTERVAL		30000
#define BACKGROUND_REQUEST_RETRY_INTERVAL				500

// Base InfoManager request class
class InfoManagerBackgroundRequest : public RandomRetryTimerObject
{
	Q_OBJECT

public:
	InfoManagerBackgroundRequest();
	virtual ~InfoManagerBackgroundRequest();

protected:
	QVariant state;
	Fault fault;

	virtual void sendRequest() = 0;
	virtual void repeatRequest();
	virtual void completeRequest(bool succeeded) = 0;

	virtual void processServerResponse(const QVariant &state, const Fault &fault);

protected slots:
	virtual void onTimerTimeout();
};

// Get user account request
class GetMyAccountRequest : public InfoManagerBackgroundRequest
{
	Q_OBJECT

public:
	GetMyAccountRequest();

signals:
	void gotMyAccountResult(QVariant state, Fault fault, UserAccount account);

protected:
	virtual void sendRequest();
	virtual void completeRequest(bool succeeded);

private:
	UserAccount account;

	void askForAccount();

private slots:
	void onGetMyAccountResult(QVariant state, Fault fault, UserAccount account);
};

// Get contacts request
class GetContactsRequest : public InfoManagerBackgroundRequest
{
	Q_OBJECT

public:
	GetContactsRequest();

signals:
	void gotContactsResult(QVariant state, Fault fault, QList<ContactInfo> result);

protected:
	virtual void sendRequest();
	virtual void completeRequest(bool succeeded);

private:
	QList<ContactInfo> contacts;

	void askForContacts();

private slots:
	void onGetContactsResult(QVariant state, Fault fault, QList<ContactInfo> result);

};

// Get contact request
class GetContactRequest : public InfoManagerBackgroundRequest
{
	Q_OBJECT

public:
	GetContactRequest(qint32 id, bool isAuthorizing);

signals:
	void gotContactResult(QVariant state, Fault fault, ContactInfo result);

protected:
	virtual void sendRequest();
	virtual void completeRequest(bool succeeded);

private:
	qint32 id;
	ContactInfo contact;

	void askForContact();

private slots:
	void onGetContactResult(QVariant state, Fault fault, ContactInfo result);

};

// Get contact status request
class GetStatusRequest : public InfoManagerBackgroundRequest
{
	Q_OBJECT

public:
	GetStatusRequest(qint32 id);

signals:
	void gotStatusResult(QVariant state, Fault fault, UserStatus::UserStatusEnum result);

protected:
	virtual void sendRequest();
	virtual void completeRequest(bool succeeded);

private:
	qint32 id;
	UserStatus::UserStatusEnum status;

	void askForStatus();

private slots:
	void onGetStatusResult(QVariant state, Fault fault, UserStatus::UserStatusEnum result);

};

// Get contact authorization request
class GetAuthorizationRequest : public InfoManagerBackgroundRequest
{
	Q_OBJECT

public:
	GetAuthorizationRequest(qint32 id);

signals:
	void gotAuthorizationResult(QVariant state, Fault fault, Authorization result);

protected:
	virtual void sendRequest();
	virtual void completeRequest(bool succeeded);

private:
	qint32 id;
	Authorization authorization;

	void askForAuthorization();

private slots:
	void onGetAuthorizationResult(QVariant state, Fault fault, Authorization result);

};

}
#endif // BACKGROUNDREQUESTS_H