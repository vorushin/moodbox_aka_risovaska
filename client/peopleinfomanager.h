#ifndef PEOPLEINFOMANAGER_H
#define PEOPLEINFOMANAGER_H

#include <QPixmap>

#include "serverproxysingleton.h"
#include "contactinfo.h"
#include "authorization.h"
#include "userinfo.h"
#include "userpictureresult.h"
#include "contactresultcode.h"
#include "notification.h"

namespace MoodBox
{

// Handy macro to access info manager singleton
#define INFOMANAGER PeopleInfoManager::getInstance()

// Userpics
#define USERPIC_FOLDER				PATH_SEPARATOR"Pictures"
#define USERPIC_DEFAULT				":/MoodBox/Resources/default_userpic.png"
#define USERPIC_DEFAULT_NAME		"Default"
#define USERPIC_THUMBNAIL_POSTFIX	"$small"

#define USERPIC_MAX_WIDTH			74
#define USERPIC_MAX_HEIGHT			74
#define USERPIC_SMALL_HOVER_WIDTH	56
#define USERPIC_SMALL_HOVER_HEIGHT	56
#define USERPIC_SMALL_WIDTH			54
#define USERPIC_SMALL_HEIGHT		54

#define USER_SPECIFIC_PARAMETERS	"Users"

#define SETTINGS_INI_FILE			"settings.ini"

// Main contact/user info management class
class PeopleInfoManager : public QObject
{
	Q_OBJECT

public:
	static PeopleInfoManager* getInstance();

public:	
	virtual ~PeopleInfoManager();
	
	/* User account management */
	void loggedOn();
	inline bool getIsLoggedOn() const { return isLoggedOn; };
	void loggedOut();

	void changeIsOnline(bool isOnline);
	inline bool isUserOnline() const { return isOnline; };

	void reloadData();

	void saveContacts() const;
	void restoreContacts();
	
	inline const UserAccount& getUserAccount() const { return currentUser; };
	void updateUserAccount(const UserAccount &accountInfo);

	// User status
	virtual void setStatus(UserStatus::UserStatusEnum status);
	inline UserStatus::UserStatusEnum getUserStatus() const { return currentUserStatus; };
	
	// User avatar
	virtual void setUserPicture(const QPixmap &picture);
	bool setUserPicture(const QString &fileName);
	void getUserPicture(QPixmap &picture, bool thumbnail = false);
	
	/* Contact list management */
	void fadeContactList();

	// Get contact by id
	ContactInfo* getContact(qint32 id) const;
	
	// Get id by login
	qint32 getContactId(const QString &login);

	// Get contact list
	QList<ContactInfo*> getContacts() const { return contactMap.values(); };

	// Person (contact/current user) name
	virtual QString getPersonName(qint32 id) const;

	// Contact status
	virtual UserStatus::UserStatusEnum getContactStatus(qint32 id) const;

	// Contact avatar
	virtual void getContactPicture(qint32 id, QPixmap &picture, bool thumbnail = false);

	// Contact motto
	virtual QString getContactMotto(qint32 id) const;

	// Contact authorization
	virtual AuthorizationState::AuthorizationStateEnum getContactAuthorization(qint32 id) const;

	// Add new contact to the list
	void addToContactList(const ContactInfo &contact);

	// Remove contact from list
	void removeFromContactList(qint32 id);

	// Set contact authorization status to Authorized
	void authorizeContact(qint32 id);

	/* Settings */
	// Path to user settings directory
	QString getUserSettingsFolder() const;

	// Path to user pictures directory
	QString getUserPicturesFolder() const;

	/* Misc */
	// User picture helper
	static void resizePicture(QPixmap &picture, bool thumbnail = false);

	// Check for known user
	bool isKnownPerson(qint32 id) const;

signals:
	void isOnlineChanged();

	// User account changes
	void userStatusChanged(UserStatus::UserStatusEnum status);
	void userPictureChanged();
	
	void userAccountUpdated();
	
	// Contact info changed
	void contactStatusChanged(qint32 id, UserStatus::UserStatusEnum status);
	void contactAuthorizationChanged(qint32 id, AuthorizationState::AuthorizationStateEnum authorizationState);
	void contactMottoChanged(qint32 id, const QString &motto);
	void contactNameChanged(qint32 id, const QString &name);
	void contactPictureChanged(qint32 id);

	void contactInfoChanged(qint32 id);

	// Contact list is changed
	void contactListChanged();

public slots:
	void onGetMyAccountResult(QVariant state, Fault fault, UserAccount account);

	void onGetContactsResult(QVariant state, Fault fault, QList<ContactInfo> result);
	void onGetContactResult(QVariant state, Fault fault, ContactInfo result);

	void onGetStatusResult(QVariant state, Fault fault, UserStatus::UserStatusEnum result);

	void onGetAuthorizingContactResult(QVariant state, Fault fault, ContactInfo result);
	void onGetAuthorizationResult(QVariant state, Fault fault, Authorization result);

private slots:
	void onGetNotifications(QList <Notification> notifications);

private:
	// Current user info
	UserAccount currentUser;

	// Current user status
	UserStatus::UserStatusEnum currentUserStatus;

	// Account status
	bool isLoggedOn;
	bool isOnline;

	// List of contacts
	QMap <qint32, ContactInfo*> contactMap;

	PeopleInfoManager();

	// Clear only contact list
	void clearContactList();

	// Clear all environment
	void clear();

	// Account operations
	// Request user account from server
	void requestMyAccount();

	// Change account info
	void changeAccountInfo(const UserAccount &info);

	// Contact list operations
	// Request contacts from server
	void requestContactList();

	// Load contact list from server response
	void loadContactList(const QList<ContactInfo> contacts);

	// Request contact info
	void requestContactInfo(qint32 id);
	void changeContactInfo(const ContactInfo &contactInfo);

	// Request new contact status
	void requestContactStatus(qint32 id);
	void changeContactStatus(qint32 id, UserStatus::UserStatusEnum status);

	// Request new authorization state
	void requestContactAuthorization(qint32 id);
	void requestAuthorizingContactInfo(qint32 id);
	void changeContactAuthorization(qint32 id, const Authorization &authorization);
	void changeContactAuthorization(qint32 id, const AuthorizationState::AuthorizationStateEnum state);
	void addAuthorizingContact(const ContactInfo &contactInfo);

	// Update contact pixmap
	void changeKnownPersonPicture(const QPixmap &picture, qint32 id, const QDateTime &dt = QDateTime());

	static PeopleInfoManager* instance;
};

}

#endif // PEOPLEINFOMANAGER_H
