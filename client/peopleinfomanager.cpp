#include "peopleinfomanager.h"

#include <QWidget>
#include <QApplication>
#include <QByteArray>
#include <QVariant>

#include "messagemanager.h"
#include "backgroundrequests.h"
#include "event.h"

#include "apptools.h"
#include "picturefilecache.h"
#include "uitools.h"

#include "international.h"
#include "common.h"
#include "debug.h"

#include "logondataprovider.h"

namespace MoodBox
{

// Comment to hide debug
//#define SHOW_PIM_DEBUG					true

#if (defined(UDEBUG)) && (defined(SHOW_PIM_DEBUG))
#define PIMDEBUG(x)	QDEBUG(x)
#else
#define PIMDEBUG(x)
#endif

// Singleton
PeopleInfoManager* PeopleInfoManager::instance = NULL;

PeopleInfoManager* PeopleInfoManager::getInstance()
{
	if (instance == NULL)
		instance = new PeopleInfoManager();
	
	return instance;
}

PeopleInfoManager::PeopleInfoManager()
: QObject(), currentUserStatus(UserStatus::Offline), isLoggedOn(false), isOnline(false)
{
	connect(SERVER, SIGNAL(notificationResult(QList<Notification>)), this, SLOT(onGetNotifications(QList<Notification>)));
}

PeopleInfoManager::~PeopleInfoManager()
{	
	clear();
}

// User account management
void PeopleInfoManager::loggedOn()
{
	Q_ASSERT_X(!getIsLoggedOn(), "PeopleInfoManager::loggedOn()", "Already logged on");
	
	// TODO load from cache by id
	QList<qint32> moderateContacts;
	changeAccountInfo(UserAccount(SERVER->getUserId(), LOGONPROVIDER->getCurrentLogin(), QString(), QDateTime(), QString(), QString(), QString(), QLocale::AnyCountry, QString(), QString(), Sex::Undefined, QDate(), Language::Undefined, false, false, false, "", Role::Undefined, moderateContacts));

	isLoggedOn = true;

	PictureFileCache::setCacheDiskFolder(getUserPicturesFolder());
}

void PeopleInfoManager::loggedOut()
{
	Q_ASSERT_X(getIsLoggedOn(), "PeopleInfoManager::loggedOut()", "Not logged on");

	clear();
	isLoggedOn = false;
}

void PeopleInfoManager::changeIsOnline(bool isOnline)
{
	Q_ASSERT_X(getIsLoggedOn(), "PeopleInfoManager::changeIsOnline", "Not logged on but changes isOnline");

	this->isOnline = isOnline;

	emit isOnlineChanged();
}

void PeopleInfoManager::reloadData()
{
	restoreContacts();

	requestMyAccount();
	requestContactList();
}

void PeopleInfoManager::restoreContacts()
{
	QSettings settings(getUserSettingsFolder() + SETTINGS_INI_FILE, QSettings::IniFormat);
	int size = settings.beginReadArray("contacts");

	clearContactList();

	for (int i = 0; i < size; ++i) 
	{
		settings.setArrayIndex(i);

		ContactInfo *newContact = new ContactInfo(settings.value("UserId").toInt(), settings.value("Login").toString(), 
											  UserStatus::Offline, settings.value("Motto").toString(), 
											  settings.value("Name").toString(), settings.value("BirthDay").toDate(), 
											  (AuthorizationState::AuthorizationStateEnum)settings.value("AuthorizationState").toInt(),
											  settings.value("Message").toString(), settings.value("IsBlocked").toBool(),
										      (ContactType::ContactTypeEnum)settings.value("Type").toInt());
		contactMap.insert(newContact->getUserId(), newContact);
	}
	settings.endArray();

	emit contactListChanged();
}

void PeopleInfoManager::saveContacts() const
{
	QSettings settings(getUserSettingsFolder() + SETTINGS_INI_FILE, QSettings::IniFormat);
	settings.beginWriteArray("contacts");

	QMap <qint32, ContactInfo*>::const_iterator contactIterator = contactMap.begin();
	int i = 0;
	while (contactIterator != contactMap.end())
	{
		settings.setArrayIndex(i);
		settings.setValue("AuthorizationState", contactIterator.value()->getAuthorizationState());
		settings.setValue("BirthDay", contactIterator.value()->getBirthDay());
		settings.setValue("IsBlocked", contactIterator.value()->getIsBlocked());
		settings.setValue("Login", contactIterator.value()->getLogin());
		settings.setValue("Message", contactIterator.value()->getMessage());
		settings.setValue("Motto", contactIterator.value()->getMotto());
		settings.setValue("Name", contactIterator.value()->getName());
		settings.setValue("Type", contactIterator.value()->getType());
		settings.setValue("UserId", contactIterator.value()->getUserId());
				
		++contactIterator;
		i++;
	}
	settings.endArray();
}

void PeopleInfoManager::updateUserAccount(const UserAccount &accountInfo)
{
	if (accountInfo.getId() != currentUser.getId())
		return;
	
	changeAccountInfo(accountInfo);
}

void PeopleInfoManager::setStatus(UserStatus::UserStatusEnum status)
{
	if (currentUserStatus == status)
		return;

	currentUserStatus = status;

	emit userStatusChanged(status);
}

void PeopleInfoManager::setUserPicture(const QPixmap &picture)
{
	if (!isUserOnline())
		return;

	changeKnownPersonPicture(picture, currentUser.getId(), QDateTime::currentDateTime());
}

bool PeopleInfoManager::setUserPicture(const QString &fileName)
{
	QPixmap tmpPic;

	if (tmpPic.load(fileName))
	{
		setUserPicture(tmpPic);
		return true;
	}

	return false;
}

void PeopleInfoManager::getUserPicture(QPixmap &picture, bool thumbnail)
{
	getContactPicture(currentUser.getId(), picture, thumbnail);
}

// Contact list management
ContactInfo* PeopleInfoManager::getContact(qint32 id) const
{
	return (contactMap.contains(id)) ? contactMap[id] : NULL;
}

qint32 PeopleInfoManager::getContactId(const QString &login)
{
	QMapIterator<qint32, ContactInfo*> i(contactMap);

	while (i.hasNext()) 
	{
		i.next();
		ContactInfo* contact = i.value();
		if (contact->getLogin() == login)
			return contact->getUserId(); 
	}

	return -1;
}

UserStatus::UserStatusEnum PeopleInfoManager::getContactStatus(qint32 id) const
{
	const ContactInfo *user = getContact(id);

	return (user != NULL) ? user->getStatus() : UserStatus::Offline;
}

QString PeopleInfoManager::getPersonName(qint32 id) const
{
	QString name;

	if (contactMap.contains(id))
		name = contactMap[id]->getDisplayName();
	else
		if (!currentUser.isNull() && currentUser.getId() == id)
			name = currentUser.getDisplayName();

	return name;
}

void PeopleInfoManager::getContactPicture(qint32 id, QPixmap &picture, bool thumbnail)
{
	static QPixmap defaultPicture(USERPIC_DEFAULT);

	QString pictureName = QString::number(id);

	PictureFileCache::find(pictureName, picture);

	if (picture.isNull())
		picture = defaultPicture;

	resizePicture(picture, thumbnail);
}

QString PeopleInfoManager::getContactMotto(qint32 id) const
{
	const ContactInfo *user = getContact(id);

	return (user != NULL) ? user->getMotto() : QString();
}

AuthorizationState::AuthorizationStateEnum PeopleInfoManager::getContactAuthorization(qint32 id) const
{
	const ContactInfo *user = getContact(id);

	return (user != NULL) ? user->getAuthorizationState() : AuthorizationState::Undefined;
}

void PeopleInfoManager::addToContactList(const ContactInfo &contact)
{
	if (getContact(contact.getUserId()) == NULL)
	{
		ContactInfo *newContact = new ContactInfo();
		*newContact = contact;
		contactMap.insert(contact.getUserId(), newContact);

		saveContacts();

		PIMDEBUG("PeopleInfoManager::contact added " << contact.getUserId());
		emit contactListChanged();
	}
}

void PeopleInfoManager::removeFromContactList(qint32 id)
{
	ContactInfo *oldContact = contactMap[id];
	contactMap.remove(id);	

	if (oldContact != NULL)
		delete oldContact;

	saveContacts();

	PIMDEBUG("PeopleInfoManager::contact removed " << id);
	emit contactListChanged();
}

void PeopleInfoManager::authorizeContact(qint32 id)
{
	PIMDEBUG("PeopleInfoManager::authorizeContact " << id);

	changeContactAuthorization(id, AuthorizationState::Authorized);

	// Request contact info
	requestContactInfo(id);
}

QString PeopleInfoManager::getUserSettingsFolder() const
{
	Q_ASSERT_X(getIsLoggedOn(), "PeopleInfoManager::getUserSettingsFolder", "not logged on");

	return AppTools::getAppDataFolder(QString::number(currentUser.getId()));
}

QString PeopleInfoManager::getUserPicturesFolder() const
{
	return getUserSettingsFolder() + CACHE_FOLDER_NAME;
}

void PeopleInfoManager::resizePicture(QPixmap &picture, bool thumbnail)
{
	int width = (thumbnail) ? USERPIC_SMALL_WIDTH : USERPIC_MAX_WIDTH - 2;
	int height = (thumbnail) ? USERPIC_SMALL_HEIGHT : USERPIC_MAX_HEIGHT - 2;

	if (picture.width() > width || picture.height() > height)
	{
		picture = picture.scaled(width, height, Qt::KeepAspectRatioByExpanding, Qt::SmoothTransformation);
		int X1 = (picture.width() > width) ? (picture.width() - width) / 2 : 0;
		int Y1 = (picture.height() > height) ? (picture.height() - height) / 2 : 0;
		if (X1 > 0 || Y1 > 0)
		{
			picture = picture.copy(QRect(X1, Y1, width, height));
		}
	}
}

bool PeopleInfoManager::isKnownPerson(qint32 id) const
{
	return contactMap.contains(id) || (currentUser.getId() == id);
}

// Network replies
void PeopleInfoManager::onGetMyAccountResult(QVariant state, Fault fault, UserAccount account)
{
	Q_UNUSED(state)

	if (fault.isNull())
	{
		changeAccountInfo(account);
	}
}

void PeopleInfoManager::onGetContactsResult(QVariant state, Fault fault, QList<ContactInfo> result)
{
	Q_UNUSED(state)

	if (fault.isNull())
	{
		loadContactList(result);

		PIMDEBUG("PeopleInfoManager::onGetContactsResult, contact list loaded ");
		emit contactListChanged();
	}
}

void PeopleInfoManager::onGetContactResult(QVariant state, Fault fault, ContactInfo result)
{

#ifdef SHOW_PIM_DEBUG
	qint32 id = state.toInt();
	PIMDEBUG("PeopleInfoManager::onGetContactResult " << id);
#else
	Q_UNUSED(state)
#endif

	if (fault.isNull())
	{
		changeContactInfo(result);
	}
}

void PeopleInfoManager::onGetStatusResult(QVariant state, Fault fault, UserStatus::UserStatusEnum result)
{
	qint32 id = state.toInt();
	PIMDEBUG("PeopleInfoManager::onGetStatusResult " << id);

	if (fault.isNull())
	{
		changeContactStatus(id, result);
	}
}

void PeopleInfoManager::onGetAuthorizingContactResult(QVariant state, Fault fault, ContactInfo result)
{

#ifdef SHOW_PIM_DEBUG
	qint32 id = state.toInt();
	PIMDEBUG("PeopleInfoManager::onGetAuthorizingContactResult " << id);
#else
	Q_UNUSED(state)
#endif

	if (fault.isNull())
	{
		addAuthorizingContact(result);
	}
}

void PeopleInfoManager::onGetAuthorizationResult(QVariant state, Fault fault, Authorization result)
{
	qint32 id = state.toInt();

	PIMDEBUG("PeopleInfoManager::onGetAuthorizationResult " << id);

	if (fault.isNull())
	{
		changeContactAuthorization(id, result);
	}
}

void PeopleInfoManager::onGetNotifications(QList <Notification> notifications)
{
	foreach (Notification notification, notifications)
	{
		switch (notification.getEvent())
		{
			case Event::StatusChanged : 
				requestContactStatus(notification.getUserId());
				break;

			case Event::ContactChanged :
				requestContactInfo(notification.getUserId());
				break;
			
			case Event::AuthorizationChanged : 
				PIMDEBUG("PeopleInfoManager::AuthorizationChanged notification");
				requestContactAuthorization(notification.getUserId()); 
				break;
			
			case Event::Reload : 
				reloadData();
				break;

			case Event::Disconnect :
				// MainWindow receives this event directly from SERVER using onServerError
				break;
		}
	}
}

void PeopleInfoManager::clearContactList()
{
	qDeleteAll(contactMap.begin(), contactMap.end());
	contactMap.clear();
}

void PeopleInfoManager::clear()
{
	clearContactList();
	currentUser = UserAccount();
}

void PeopleInfoManager::requestMyAccount()
{
	new GetMyAccountRequest();
}

void PeopleInfoManager::changeAccountInfo(const UserAccount &accountInfo)
{
	currentUser = accountInfo;

	emit userAccountUpdated();
}

void PeopleInfoManager::requestContactList()
{
	new GetContactsRequest();
}

void PeopleInfoManager::fadeContactList()
{
	foreach(ContactInfo *contact, contactMap.values())
	{
		contactMap[contact->getUserId()]->setStatus(UserStatus::Offline);
		emit contactStatusChanged(contact->getUserId(), UserStatus::Offline);
	}
}

void PeopleInfoManager::loadContactList(const QList<ContactInfo> contacts)
{
	clearContactList();

 	foreach(ContactInfo contact, contacts)
	{
		ContactInfo *newContact	= new ContactInfo();
		*newContact = contact;
		contactMap.insert(newContact->getUserId(), newContact);
	}

	saveContacts();
}

void PeopleInfoManager::requestContactInfo(qint32 id)
{
	new GetContactRequest(id, false);
}

void PeopleInfoManager::changeContactInfo(const ContactInfo &contactInfo)
{
	ContactInfo *contact = getContact(contactInfo.getUserId());

	if (contact == NULL)
		return;

	bool isStatusChanged = contact->getStatus() != contactInfo.getStatus();
	bool isMottoChanged = contact->getMotto() != contactInfo.getMotto();
	bool isNameChanged = contact->getName() != contactInfo.getName();

	contact->setStatus(contactInfo.getStatus());
	contact->setMotto(contactInfo.getMotto());
	contact->setName(contactInfo.getName());

	saveContacts();

	if (isStatusChanged)
		emit contactStatusChanged(contactInfo.getUserId(), contactInfo.getStatus());

	if (isMottoChanged)
		emit contactMottoChanged(contactInfo.getUserId(), contactInfo.getMotto());

	if (isNameChanged)
		emit contactNameChanged(contactInfo.getUserId(), contactInfo.getName());
	
	emit contactInfoChanged(contactInfo.getUserId());
}

void PeopleInfoManager::requestContactStatus(qint32 id)
{
	new GetStatusRequest(id);
}

void PeopleInfoManager::changeContactStatus(qint32 id, UserStatus::UserStatusEnum status)
{
	ContactInfo *contact = getContact(id);

	if (contact == NULL)
		return;

	contact->setStatus(status);

	emit contactStatusChanged(id, status);
}

void PeopleInfoManager::requestContactAuthorization(qint32 id)
{
	PIMDEBUG("PeopleInfoManager::requestContactAuthorization " << id);
	new GetAuthorizationRequest(id);
}

void PeopleInfoManager::requestAuthorizingContactInfo(qint32 id)
{
	PIMDEBUG("PeopleInfoManager::requestAuthorizingContactInfo " << id);
	new GetContactRequest(id, true);
}

void PeopleInfoManager::changeContactAuthorization(qint32 id, const Authorization &authorization)
{
	PIMDEBUG("PeopleInfoManager::changeContactAuthorization " << id);

	ContactInfo *contact = getContact(id);

	if (contact == NULL)
	{
		// Asked for authorization?
		if (authorization.getState() == AuthorizationState::WaitsAuthorizationFromMe)
			requestAuthorizingContactInfo(id);
		
		return;
	}

	PIMDEBUG("PeopleInfoManager::changeContactAuthorization - setAuthorizationState " << authorization.getState());

	QByteArray message;
	message.append(authorization.getMessage());

	contact->setMessage(message);

	// Need to update contact status
	if (authorization.getState() == AuthorizationState::Authorized)
		requestContactStatus(id);

	changeContactAuthorization(id, authorization.getState());
}

void PeopleInfoManager::changeContactAuthorization(qint32 id, const AuthorizationState::AuthorizationStateEnum state)
{
	ContactInfo *contact = getContact(id);

	if (contact == NULL || contact->getAuthorizationState() == state)
		return;

	contact->setAuthorizationState(state);

	saveContacts();

	// Request contact info
	requestContactInfo(id);

	emit contactAuthorizationChanged(id, state);
}

void PeopleInfoManager::addAuthorizingContact(const ContactInfo &contactInfo)
{
	PIMDEBUG("PeopleInfoManager::addAuthorizingContact " << contactInfo.getUserId());

	addToContactList(contactInfo);

	emit contactAuthorizationChanged(contactInfo.getUserId(), contactInfo.getAuthorizationState());
}

void PeopleInfoManager::changeKnownPersonPicture(const QPixmap &picture, qint32 id, const QDateTime &dt)
{
	if (!isKnownPerson(id))
		return;

	QString pictureName = QString::number(id);

	if (picture.isNull())
	{
		PictureFileCache::remove(pictureName);
		PictureFileCache::insert(pictureName, picture, dt);
	}
	else
	{
		QPixmap tmpPicture = picture;
		resizePicture(tmpPicture);
		PictureFileCache::insert(pictureName, tmpPicture, dt);
	}

	if (id == currentUser.getId())
		emit userPictureChanged();
	else
		emit contactPictureChanged(id);
}

}
