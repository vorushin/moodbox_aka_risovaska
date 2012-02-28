#ifndef NEWCONTACTLISTWIDGET2_H
#define NEWCONTACTLISTWIDGET2_H

#include <QItemDelegate>
#include <QLabel>
#include <QListView>
#include <QMenu>
#include <QStandardItemModel>
#include <QSortFilterProxyModel>

#include <QString>

#include "userstatus.h"
#include "authorizationstate.h"
#include "contactinfo.h"
#include "uitools.h"

namespace MoodBox
{

#define ALL_FRIENDS_CONTACT					QT_TRANSLATE_NOOP("MoodBox::NewContactListWidget2", "AllFriendsContact")
#define ALL_FRIENDS_CONTACT_DESCRIPTION		QT_TRANSLATE_NOOP("MoodBox::NewContactListWidget2", "AllFriendsContactDescription")
#define ALL_FRIENDS_CONTACT_CAPTION			QT_TRANSLATE_NOOP("MoodBox::NewContactListWidget2", "AllFriendsContactCaption")
#define MY_SANDBOX_CONTACT					QT_TRANSLATE_NOOP("MoodBox::NewContactListWidget2", "MySandboxContact")
#define MY_SANDBOX_CONTACT_DESCRIPTION		QT_TRANSLATE_NOOP("MoodBox::NewContactListWidget2", "MySandboxContactDescription")
#define MY_SANDBOX_CONTACT_CAPTION			QT_TRANSLATE_NOOP("MoodBox::NewContactListWidget2", "MySandboxContactCaption")

// тип элемента списка контактов
#define CONTACT_LIST_ITEM_DATA		(Qt::UserRole + 1)
#define CONTACT_LIST_ITEM_DATA2		(Qt::UserRole + 2)

#define CONTACT_ITEM_HEIGHT		24

// Context menu
#define AUTHORIZE_MENU_ITEM						QT_TRANSLATE_NOOP("MoodBox::NewContactListWidget2", "AuthorizeMenuItem")
#define REQUEST_AUTHORIZATION_MENU_ITEM			QT_TRANSLATE_NOOP("MoodBox::NewContactListWidget2", "RequestAuthorizationMenuItem")
#define DELETE_MENU_USER_ITEM					QT_TRANSLATE_NOOP("MoodBox::NewContactListWidget2", "DeleteMenuUserItem")
#define DELETE_MENU_CHANNEL_ITEM				QT_TRANSLATE_NOOP("MoodBox::NewContactListWidget2", "DeleteMenuChannelItem")
#define CHANNEL_INFO_MENU_ITEM					QT_TRANSLATE_NOOP("MoodBox::NewContactListWidget2", "ChannelInfoMenuItem")
#define CHANNEL_NOTIFY_MENU_ITEM				QT_TRANSLATE_NOOP("MoodBox::NewContactListWidget2", "ChannelNotifyMenuItem")

class ContactListItemDelegate;

class NewContactListWidget2 : public QListView
{
	Q_OBJECT

public:
	NewContactListWidget2(QWidget *parent);

	bool getContactInfo(ContactInfo &contact, const int i) const { return getContactInfo(contact, itemModel->item(i)->data(CONTACT_LIST_ITEM_DATA)); };
	static bool getContactInfo(ContactInfo &contact, const QModelIndex &index) { return getContactInfo(contact, index.data(CONTACT_LIST_ITEM_DATA)); };
	static bool getContactInfo(ContactInfo &contact, const QVariant &data);
	bool getSelectedContact(ContactInfo& contact) const { return getContactInfo(contact, currentIndex()); };

	QModelIndex getContactIndex(qint32 id) const;
	QModelIndex getAllFriendsContactIndex() const;

	int getUnreadMessagesForContact(qint32 id) const;
	int getUnreadMessagesForAllFriends() const;

	void onMessageReceived(qint32 userId, bool isAllFriends = false);
	
	static int getUnreadMessages(const QModelIndex &index);

	void clearContacts() { itemModel->clear(); }

	const bool dragged() const { return isDragged; }

signals:
	void contactSelected(qint32 id);
	void contactImageDrop(qint32 id, const QImage &image);
	void unreadContacts(int);

public slots:
	void onContactListChanged();
	void onActivated(const QModelIndex &index);
	ContactInfo onContactChanged(qint32 id);
	void onContactAuthorizationChanged(qint32, AuthorizationState::AuthorizationStateEnum);
	void onAuthorizationDialogApproveFinished(int result);

	void onOnline();
	void onOffline();

	void onUserInfoTriggered();
	void onChannelInfoTriggered();
	void onDeleteTriggered();
	void onAuthorizeTriggered();
	void onRequestAuthorizationTriggered();
	void onChannelNotifyTriggered();

protected:
	void contextMenuEvent(QContextMenuEvent *event);
	
	void dragEnterEvent(QDragEnterEvent *event);
	void dropEvent(QDropEvent *event);
	void dragLeaveEvent(QDragLeaveEvent *event);

	virtual void closeEvent(QCloseEvent *event);
	virtual void leaveEvent(QEvent *event);

	bool canUseMimeData(const QMimeData *data) const;
	QString getImageFileFromMimeData(const QMimeData *data) const;

protected slots:
	void currentChanged(const QModelIndex &current, const QModelIndex &previous);
	
	void onItemHovered(const QModelIndex &index);

private:
	ContactInfo allFriendsContact;

	QStandardItemModel *itemModel;
	ContactListItemDelegate *itemDelegate;
	QSortFilterProxyModel *proxyModel;

	QMenu *contextMenu;
	QAction *authorizeAction;
	QAction *requestAuthorizationAction;
	QAction *userInfoAction;
	QAction *removeUserAction;
	QAction *removeChannelAction;
	QAction *channelNotifyAction;
	QAction *channelInfoAction;	

	bool isDragged;

	void connectInfoManager();

	int getUnreadCount() const;
	int getUnreadMessages(const int i) const;
};

// элемент списка контактов
class ContactListItem: public QStandardItem
{
public:
	ContactListItem(ContactInfo contact, QListView *parent);

	virtual int type () const { return CONTACT_LIST_ITEM_DATA;};
	virtual bool operator< (const QStandardItem & other) const;

protected:
	QListView* parentList;
};

// делегат для отображения элемента списка контактов
class ContactListItemDelegate : public QItemDelegate
{
    Q_OBJECT

public:
    ContactListItemDelegate(NewContactListWidget2 *parent): QItemDelegate(parent), parentList(parent), hoveredIndexSet(false) { };

    virtual void paint(QPainter *painter, const QStyleOptionViewItem &option, const QModelIndex &index) const;
    virtual QSize sizeHint(const QStyleOptionViewItem &option, const QModelIndex &index) const;
	virtual QWidget *createEditor(QWidget*, const QStyleOptionViewItem&, const QModelIndex&) { return NULL; }; 

	void setHoveredIndex(const QModelIndex &index);
	inline QModelIndex getHoveredIndex() const { return hoveredIndex; };
	void clearHoveredIndex();

private:
	void paintContact(QPainter *painter, ContactInfo &contact, const QRect &rect, const QIcon &userIcon, 
		bool selected, bool mouseOver, int unreadMessages, int row) const;

private:
	NewContactListWidget2* parentList;
	QPersistentModelIndex hoveredIndex;
	bool hoveredIndexSet;
};

// модель списка - для drag & drop
class ContactListModel : public QStandardItemModel
{
	Q_OBJECT

public:
	ContactListModel(QObject *parent = 0) : QStandardItemModel(parent) {}

	bool dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int, const QModelIndex &);

	Qt::ItemFlags flags(const QModelIndex &index) const
	{
		if (index.isValid())
			return Qt::ItemIsDropEnabled | Qt::ItemIsSelectable | Qt::ItemIsEnabled;
		else 
			return Qt::NoItemFlags;
	}

	Qt::DropActions supportedDropActions() const
	{
		return Qt::ActionMask;
	}

	QStringList mimeTypes () const 
	{
		static QStringList types;

		if (types.isEmpty())
		{
			types << "application/x-qt-image" << "image/gif" << "image/png" << "image/jpeg" << "image/bmp" << "image/tiff" << "text/uri-list";
		}

		return types;
	}
};

}

#endif // NEWCONTACTLISTWIDGET2_H
