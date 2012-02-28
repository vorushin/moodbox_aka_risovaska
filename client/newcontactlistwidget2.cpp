#include "newcontactlistwidget2.h"

#include <QMenu>
#include <QMouseEvent>
#include <QPainter>
#include <QUrl>

#include "authorizationdialog.h"
#include "contactinfodialog.h"
#include "debug.h"
#include "peopleinfomanager.h"
#include "removecontactdialog.h"
#include "uitools.h"
#include "channelinfomanager.h"

namespace MoodBox 
{

NewContactListWidget2::NewContactListWidget2(QWidget *parent)
	:QListView(parent), contextMenu(NULL), isDragged(false)
{
	allFriendsContact = ContactInfo::allFriendsContact();
	allFriendsContact.setLogin(tr(ALL_FRIENDS_CONTACT));

	itemModel = new ContactListModel(this);
	itemModel->setColumnCount(1);

	proxyModel = new QSortFilterProxyModel(this);
	proxyModel->setSourceModel(itemModel);
	proxyModel->setDynamicSortFilter(true);
	proxyModel->setFilterCaseSensitivity(Qt::CaseInsensitive);
	proxyModel->setFilterKeyColumn(0);

	itemDelegate = new ContactListItemDelegate(this);
	setItemDelegate(itemDelegate);
	setModel(proxyModel);

	connectInfoManager();

	connect(this, SIGNAL(activated(const QModelIndex&)), this, SLOT(onActivated(const QModelIndex&)));
	connect(this, SIGNAL(entered(const QModelIndex &)), this, SLOT(onItemHovered(const QModelIndex &)));

	// context menu actions
	authorizeAction = new QAction(tr(AUTHORIZE_MENU_ITEM), this);
	connect(authorizeAction, SIGNAL(triggered()), this, SLOT(onAuthorizeTriggered()));

	requestAuthorizationAction = new QAction(tr(REQUEST_AUTHORIZATION_MENU_ITEM), this);
	connect(requestAuthorizationAction, SIGNAL(triggered()), this, SLOT(onRequestAuthorizationTriggered()));

	userInfoAction = new QAction(tr(USER_INFO_MENU_ITEM), this);
	connect(userInfoAction, SIGNAL(triggered()), this, SLOT(onUserInfoTriggered()));

	removeUserAction = new QAction(tr(DELETE_MENU_USER_ITEM), this);
	connect(removeUserAction, SIGNAL(triggered()), this, SLOT(onDeleteTriggered()));

	removeChannelAction = new QAction(tr(DELETE_MENU_CHANNEL_ITEM), this);
	connect(removeChannelAction, SIGNAL(triggered()), this, SLOT(onDeleteTriggered()));

	channelNotifyAction = new QAction(tr(CHANNEL_NOTIFY_MENU_ITEM), this);
	channelNotifyAction->setCheckable(true);
	connect(channelNotifyAction, SIGNAL(triggered()), this, SLOT(onChannelNotifyTriggered()));

	channelInfoAction = new QAction(tr(CHANNEL_INFO_MENU_ITEM), this);
	connect(channelInfoAction, SIGNAL(triggered()), this, SLOT(onChannelInfoTriggered()));

	setDragDropMode(QAbstractItemView::DropOnly);
	setDragDropOverwriteMode(false);
	setDragEnabled(false);
	setDropIndicatorShown(true);
	setAcceptDrops(true);
	setMouseTracking(true);
    
#ifdef Q_WS_MAC    
    setAttribute(Qt::WA_MacShowFocusRect, false);        
#endif
}

bool NewContactListWidget2::getContactInfo(ContactInfo &contact, const QVariant &data)
{
   if (!data.canConvert<ContactInfo>()) 
		return false;
	
	contact = data.value<ContactInfo>();
	return true;
}

QModelIndex NewContactListWidget2::getContactIndex(qint32 id) const
{
	for (int i = 0; i < itemModel->rowCount(); i++)
	{
		ContactInfo contact;
		getContactInfo(contact, i);

		if (contact.getUserId() == id)
			return itemModel->index(i, 0);
	}

	return QModelIndex();
}

QModelIndex NewContactListWidget2::getAllFriendsContactIndex() const
{
	for (int i = 0; i < itemModel->rowCount(); i++)
	{
		ContactInfo contact;
		getContactInfo(contact, i);

		if (contact.isAllFriendsContact())
			return itemModel->index(i, 0);
	}

	return QModelIndex();
}

int NewContactListWidget2::getUnreadMessagesForContact(qint32 id) const
{
	QModelIndex index = getContactIndex(id);

	return (index.isValid()) ? getUnreadMessages(index) : 0;
}

int NewContactListWidget2::getUnreadMessagesForAllFriends() const
{
	QModelIndex index = getAllFriendsContactIndex();

	return (index.isValid()) ? getUnreadMessages(index) : 0;
}

void NewContactListWidget2::onMessageReceived(qint32 userId, bool isAllFriends)
{
	ContactInfo selectedContact;
	getSelectedContact(selectedContact);

	if (isAllFriends && selectedContact.isAllFriendsContact())
		return;

	if (!isAllFriends && (selectedContact.getUserId() == userId))
		return;

	if (isAllFriends)
	{
		itemModel->item(0)->setData(getUnreadMessages(0) + 1, CONTACT_LIST_ITEM_DATA2);
		emit unreadContacts(getUnreadCount());
	}
	else 
	{
		for (int i = 0; i < itemModel->rowCount(); i++)
		{
			ContactInfo contact;
			getContactInfo(contact, i);

			if (contact.getUserId() == userId)
			{
				itemModel->item(i)->setData(getUnreadMessages(i) + 1, CONTACT_LIST_ITEM_DATA2);
				emit unreadContacts(getUnreadCount());
				break;
			}
		}
	}
}

void NewContactListWidget2::onContactListChanged()
{
	QDEBUG("onContactListChanged()");

	bool hasAllFriendsItem = false;
	bool hasMySandboxItem = false;
	for (int i = itemModel->rowCount() - 1; i >= 0; --i)
	{
		ContactInfo contact;
		getContactInfo(contact, i);

		if(contact.isAllFriendsContact())
		{
			hasAllFriendsItem = true;
		}
		else if (contact.getUserId() == INFOMANAGER->getUserAccount().getId())
		{
			hasMySandboxItem = true;
		}
		else
		{
			bool found = false;

			foreach(ContactInfo* infoContact, INFOMANAGER->getContacts())
			{
				if (contact.getUserId() == infoContact->getUserId())
				{
					found = true;
					break;
				}
			}

			if(!found)
			{
				itemModel->removeRow(i);
			}
		}
	}

	QList<ContactInfo*> newItems;
	foreach(ContactInfo* infoContact, INFOMANAGER->getContacts())
	{
		bool found = false;

		for (int i = 0; i < itemModel->rowCount(); ++i)
		{
			ContactInfo contact;
			getContactInfo(contact, i);
			if (contact.getUserId() == infoContact->getUserId())
			{
				itemModel->item(i)->setData(QVariant::fromValue(*infoContact), CONTACT_LIST_ITEM_DATA);
				found = true;
				break;
			}
		}

		if(!found)
		{
			newItems.append(infoContact);
		}
	}

	foreach(ContactInfo* contact, newItems)
	{
		itemModel->appendRow(new ContactListItem(*contact, this));
	}

	if(!hasAllFriendsItem)
	{
		itemModel->appendRow(new ContactListItem(allFriendsContact, this));
	}

	if(!hasMySandboxItem)
	{
		ContactInfo mySandboxContact = ContactInfo::mySandboxContact();
		mySandboxContact.setLogin(tr(MY_SANDBOX_CONTACT));
		itemModel->appendRow(new ContactListItem(mySandboxContact, this));
	}

	if(!currentIndex().isValid())
	{
		QModelIndex index;
		for (int i = itemModel->rowCount() - 1; i >= 0; --i)
		{
			ContactInfo contact;
			getContactInfo(contact, i);

			if(contact.isAllFriendsContact())
			{
				index = itemModel->item(i)->index();
				break;
			}
		}

		if(index.isValid())
		{
			setCurrentIndex(proxyModel->mapFromSource(index));
		}
	}

	itemModel->sort(0, Qt::AscendingOrder);

	emit unreadContacts(getUnreadCount());

	// do not remove commented code bellow, it will be usefull when implementing [MB-140]
/*
	itemModel->clear();

	itemModel->setRowCount(INFOMANAGER->getContacts().count() + 1);

	ContactListItem* allFriends = new ContactListItem(allFriendsContact, this);
	itemModel->setItem(0, allFriends);
	int currentRow = 1;

	foreach(ContactInfo* contact, INFOMANAGER->getContacts())
	{
		ContactListItem* item = new ContactListItem(*contact, this);
		itemModel->setItem(currentRow++, item);
	}

	itemModel->sort(0, Qt::AscendingOrder);
*/
}

ContactInfo NewContactListWidget2::onContactChanged(qint32 id)
{
	for (int i = 0; i < itemModel->rowCount(); ++i)
	{
		ContactInfo contact;
		getContactInfo(contact, i);
		if (contact.getUserId() == id)
		{
			contact = *(INFOMANAGER->getContact(id));
			itemModel->item(i)->setData(QVariant::fromValue(contact), CONTACT_LIST_ITEM_DATA);
			itemModel->sort(0, Qt::AscendingOrder);
			return contact;
		}
	}

	return ContactInfo();
}

void NewContactListWidget2::onActivated(const QModelIndex &index)
{
	ContactInfo contact;
	getContactInfo(contact, index);
	
	if (contact.getAuthorizationState() == AuthorizationState::WaitsAuthorizationFromMe)
	{
		authorizeAction->activate(QAction::Trigger);
	}
	else if (contact.getAuthorizationState() == AuthorizationState::NotAuthorizedMe)
	{
		requestAuthorizationAction->activate(QAction::Trigger);
	}
}

void NewContactListWidget2::onContactAuthorizationChanged(qint32 id, AuthorizationState::AuthorizationStateEnum authorizationState)
{
	ContactInfo contact = onContactChanged(id);

	if (authorizationState == AuthorizationState::WaitsAuthorizationFromMe)
	{
		AuthorizationDialog *authDialog = new AuthorizationDialog(this);
		connect(authDialog, SIGNAL(finished(int)), this, SLOT(onAuthorizationDialogApproveFinished(int)));
		
		authDialog->approve(contact);
	}
}

void NewContactListWidget2::onAuthorizationDialogApproveFinished(int result)
{
	Q_UNUSED(result);
	itemModel->sort(0, Qt::AscendingOrder);
}

void NewContactListWidget2::onOnline()
{
	allFriendsContact.setStatus(UserStatus::Online);

	if (itemModel->item(0) != NULL)
		itemModel->item(0)->setData(QVariant::fromValue(allFriendsContact), CONTACT_LIST_ITEM_DATA);
}

void NewContactListWidget2::onOffline()
{
	allFriendsContact.setStatus(UserStatus::Offline);
	
	if (itemModel->item(0) != NULL)
		itemModel->item(0)->setData(QVariant::fromValue(allFriendsContact), CONTACT_LIST_ITEM_DATA);
}

void NewContactListWidget2::onUserInfoTriggered()
{
	ContactInfo contact;
	if (!getSelectedContact(contact))
		return;

	// Show profile dialog
	ContactInfoDialog *contactInfoDialog = new ContactInfoDialog(contact, this);
	contactInfoDialog->show();
}

void NewContactListWidget2::onChannelInfoTriggered()
{
	ContactInfo contact;
	if (!getSelectedContact(contact))
		return;

	if (contact.isAllFriendsContact()) 
	{
		UserInfo userInfo = UserInfo(-1, tr(ALL_FRIENDS_CONTACT), tr(ALL_FRIENDS_CONTACT), QDateTime(), tr(ALL_FRIENDS_CONTACT_CAPTION), 
			tr(ALL_FRIENDS_CONTACT_DESCRIPTION),
			QLocale::AnyCountry, "", MoodBox::Sex::Undefined, QDate(), "", MoodBox::UserStatus::Undefined);
		ContactInfoDialog *contactInfoDialog = new ContactInfoDialog(userInfo, this, true);
		contactInfoDialog->show();
	}
	else if (contact.getUserId() == INFOMANAGER->getUserAccount().getId()) // My Sandbox
	{
		UserInfo userInfo = UserInfo(contact.getUserId(), tr(MY_SANDBOX_CONTACT), tr(MY_SANDBOX_CONTACT), QDateTime(), tr(MY_SANDBOX_CONTACT_CAPTION), 
			tr(MY_SANDBOX_CONTACT_DESCRIPTION),
			QLocale::AnyCountry, "", MoodBox::Sex::Undefined, QDate(), "", MoodBox::UserStatus::Undefined);
		ContactInfoDialog *contactInfoDialog = new ContactInfoDialog(userInfo, this, true);
		contactInfoDialog->show();
	}
	else if (contact.getType() == ContactType::Channel)
	{
		ContactInfoDialog *contactInfoDialog = new ContactInfoDialog(contact, this);
		contactInfoDialog->show();
	}
}

void NewContactListWidget2::onDeleteTriggered()
{
	ContactInfo contact;
	if (!getSelectedContact(contact))
		return;

	// Remove contact
	RemoveContactDialog *removeDialog = new RemoveContactDialog(contact.getUserId(), contact.getType(), this);
	removeDialog->show();
}

void NewContactListWidget2::onAuthorizeTriggered()
{
	ContactInfo contact;
	if (!getSelectedContact(contact))
		return;

	AuthorizationDialog *authDialog = new AuthorizationDialog(this);
	authDialog->approve(contact);
}

void NewContactListWidget2::onRequestAuthorizationTriggered()
{
	ContactInfo contact;
	if (!getSelectedContact(contact))
		return;

	AuthorizationDialog *authDialog = new AuthorizationDialog(this);
	authDialog->request(contact);
}

void NewContactListWidget2::onChannelNotifyTriggered()
{
	ContactInfo contact;
	if (!getSelectedContact(contact))
		return;

	CHANNELMANAGER->setChannelNotifications(contact.getUserId(), channelNotifyAction->isChecked());
}

void NewContactListWidget2::contextMenuEvent(QContextMenuEvent *event)
{	
	QMouseEvent * customEvent = new QMouseEvent(QEvent::MouseButtonPress, event->pos(), Qt::LeftButton, 
		Qt::LeftButton, event->modifiers());

	QListView::mousePressEvent(customEvent);
	delete customEvent;

	ContactInfo contact;

	if (!getSelectedContact(contact))
		return;	

	if (contextMenu == NULL)
	{
		contextMenu = new QMenu(this);
		contextMenu->addAction(authorizeAction);
		contextMenu->addAction(requestAuthorizationAction);
		contextMenu->addAction(userInfoAction);
		contextMenu->addAction(channelInfoAction);
		contextMenu->addAction(channelNotifyAction);
		contextMenu->addAction(removeUserAction);
		contextMenu->addAction(removeChannelAction);
	}

	if (contact.isAllFriendsContact() || contact.getUserId() == INFOMANAGER->getUserAccount().getId())
	{
		authorizeAction->setVisible(false);
		requestAuthorizationAction->setVisible(false);
		removeUserAction->setVisible(false);
		removeChannelAction->setVisible(false);
		channelNotifyAction->setVisible(false);
		userInfoAction->setVisible(false);
		channelInfoAction->setVisible(true);
	}
	else if (contact.getType() == ContactType::Channel)
	{
		authorizeAction->setVisible(false);
		requestAuthorizationAction->setVisible(false);
		removeUserAction->setVisible(false);
		removeChannelAction->setVisible(true);
		channelNotifyAction->setVisible(true);
		channelNotifyAction->setChecked(CHANNELMANAGER->getChannelNotifications(contact.getUserId()));
		userInfoAction->setVisible(false);
		channelInfoAction->setVisible(true);
	}
	else
	{
		if (contact.getAuthorizationState() == AuthorizationState::WaitsAuthorizationFromMe)
		{				
			authorizeAction->setVisible(true);
			requestAuthorizationAction->setVisible(false);
			contextMenu->setDefaultAction(authorizeAction);
		}
		else if (contact.getAuthorizationState() == AuthorizationState::NotAuthorizedMe)
		{
			authorizeAction->setVisible(false);
			requestAuthorizationAction->setVisible(true);
			contextMenu->setDefaultAction(requestAuthorizationAction);
		}
		else
		{
			authorizeAction->setVisible(false);
			requestAuthorizationAction->setVisible(false);
		}

		removeUserAction->setVisible(true);
		removeChannelAction->setVisible(false);
		userInfoAction->setVisible(true);
		channelInfoAction->setVisible(false);
		channelNotifyAction->setVisible(false);
	}

	contextMenu->exec(event->globalPos());
}

void NewContactListWidget2::dragEnterEvent(QDragEnterEvent *event)
{
	if (canUseMimeData(event->mimeData()))
	{
		event->acceptProposedAction();	
		isDragged = true;
	}
}

void NewContactListWidget2::dropEvent(QDropEvent *event)
{
	QImage image;

	if (event->mimeData()->hasImage()) 
	{
		image = qvariant_cast<QImage>(event->mimeData()->imageData());		
	}
	else
	{
		image.load(getImageFileFromMimeData(event->mimeData()));
	}

	if (!image.isNull())
	{
		QModelIndex droppedIndex = indexAt(event->pos());
		setCurrentIndex(droppedIndex);

		ContactInfo contact;
		getContactInfo(contact, droppedIndex);

		qint32 userId = contact.getUserId();
		event->acceptProposedAction();

		emit contactImageDrop(userId, image);
	}
	else
	{
		event->ignore();
	}

	isDragged = false;
}

void NewContactListWidget2::dragLeaveEvent(QDragLeaveEvent *event)
{
	Q_UNUSED(event)

	isDragged = false;
}

void NewContactListWidget2::closeEvent(QCloseEvent *event)
{
	QListView::closeEvent(event);
	itemDelegate->clearHoveredIndex();
}

void NewContactListWidget2::leaveEvent(QEvent *event)
{
	// If visible we should turn off highlight of item
	if (isVisible())
		onItemHovered(QModelIndex());

	QListView::leaveEvent(event);
}

bool NewContactListWidget2::canUseMimeData(const QMimeData *data) const
{
	return data->hasImage() || data->hasUrls();
}

QString NewContactListWidget2::getImageFileFromMimeData(const QMimeData *data) const
{
	if (!canUseMimeData(data))
		return QString();

	return data->urls().first().toLocalFile();
}

void NewContactListWidget2::currentChanged(const QModelIndex &current, const QModelIndex &previous)
{
	ContactInfo contact;
	getContactInfo(contact, current);

	emit contactSelected(contact.getUserId());

	if (getUnreadMessages(current.row()) > 0)
	{
		itemModel->item(current.row())->setData(0, CONTACT_LIST_ITEM_DATA2);		
		emit unreadContacts(getUnreadCount());
	}

	QListView::currentChanged(current, previous);
}

void NewContactListWidget2::onItemHovered(const QModelIndex &index)
{
	itemDelegate->setHoveredIndex(index);
}

void NewContactListWidget2::connectInfoManager()
{	
	connect(INFOMANAGER, SIGNAL(contactListChanged()), this, SLOT(onContactListChanged()));

	connect(INFOMANAGER, SIGNAL(contactStatusChanged(qint32, UserStatus::UserStatusEnum)), this, SLOT(onContactChanged(qint32)));
	connect(INFOMANAGER, SIGNAL(contactNameChanged(qint32, const QString &)), this, SLOT(onContactChanged(qint32)));
	connect(INFOMANAGER, SIGNAL(contactAuthorizationChanged(qint32, AuthorizationState::AuthorizationStateEnum)), this, SLOT(onContactAuthorizationChanged(qint32, AuthorizationState::AuthorizationStateEnum)));	
}

int NewContactListWidget2::getUnreadCount() const
{
	int count = 0;
	for(int i = 0; i < itemModel->rowCount(); i++)
		if (getUnreadMessages(i) > 0)
			count++;

	return count;
}

int NewContactListWidget2::getUnreadMessages(const int i) const
{
	QVariant data = itemModel->item(i)->data(CONTACT_LIST_ITEM_DATA2);
	if (data.canConvert<int>())
		return data.value<int>();
	else
		return 0;
}

int NewContactListWidget2::getUnreadMessages(const QModelIndex &index)
{
	QVariant data = index.data(CONTACT_LIST_ITEM_DATA2);
	if (data.canConvert<int>())
		return data.value<int>();
	else
		return 0;
}

// ContactListItem implementation
ContactListItem::ContactListItem(ContactInfo contact, QListView *parent): QStandardItem(), parentList(parent)
{
	setData(QVariant::fromValue(contact), CONTACT_LIST_ITEM_DATA);
	setText(contact.getDisplayName());

	setDropEnabled(true);
}

bool ContactListItem::operator <(const QStandardItem &other) const
{  
	// compare if element type and data type are comparable
	if (other.type() == CONTACT_LIST_ITEM_DATA)
	{
		const QVariant l = data(CONTACT_LIST_ITEM_DATA);
		const QVariant r = other.data(CONTACT_LIST_ITEM_DATA);

		if (l.canConvert<ContactInfo>() && r.canConvert<ContactInfo>())
		{
			ContactInfo lc = l.value<ContactInfo>();
			ContactInfo rc = r.value<ContactInfo>();

			return lc < rc;
		}
	}

	return QStandardItem::operator <(other);
}

// ContactListItemDelegate class
void ContactListItemDelegate::paint(QPainter *painter, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
	ContactInfo contact;

	// if no info by index draw by default
	if (!NewContactListWidget2::getContactInfo(contact, index))
	{
		QItemDelegate::paint(painter, option, index);
		return;
	}

	QRect r = option.rect;
	int unreadMessages = NewContactListWidget2::getUnreadMessages(index);

	// Qt 4.5 bug workaround
	bool isHovered = option.state & QStyle::State_MouseOver;
	if (hoveredIndexSet && !isHovered && (index == hoveredIndex) )
		isHovered = true;

	QIcon icon;
	if (contact.getAuthorizationState() != AuthorizationState::Authorized)
		icon = QIcon(NOTAUTH_STATUS_ICON);
	else if (contact.getStatus() == UserStatus::Online)
		icon = QIcon(ONLINE_STATUS_ICON);
	else if (contact.getStatus() == UserStatus::Offline)
		icon = QIcon(OFFLINE_STATUS_ICON);		
	else 
		icon = QIcon(OFFLINE_STATUS_ICON);

	paintContact(painter, contact, option.rect, icon, 
		(option.state & QStyle::State_Selected), isHovered, unreadMessages, index.row());
}

QSize ContactListItemDelegate::sizeHint(const QStyleOptionViewItem &option, const QModelIndex &index) const
{
	Q_UNUSED(option);
	Q_UNUSED(index);
    return QSize(150, CONTACT_ITEM_HEIGHT);
}

void ContactListItemDelegate::paintContact(QPainter *painter, ContactInfo &contact, const QRect &rect, const QIcon &userIcon, 
										   bool selected, bool mouseOver, int unreadMessages, int row) const
{
	painter->save();

	if (selected)
		painter->fillRect(rect, QBrush(QPixmap(":/MoodBox/Resources/cl_selected_item.png")));	
	else if ((row % 2) == 1)
		painter->fillRect(rect, QBrush(QPixmap(":/MoodBox/Resources/tv_background_light.png")));
	else 
		painter->fillRect(rect, QBrush(QPixmap(":/MoodBox/Resources/tv_background.png")));

	if (mouseOver)
		painter->fillRect(rect, QBrush(QPixmap(":/MoodBox/Resources/cl_mouse_over_item.png")));

	painter->setRenderHint(QPainter::Antialiasing, true);

	// paint a status icon
	userIcon.paint(painter, QRect(rect.left() + 5, rect.top() + 7, 11, 11));
	
	// write an user name
	QFont nameFont("Verdana");
	nameFont.setPixelSize(13);
	
	if (contact.isAllFriendsContact() || contact.getType() == ContactType::Channel ||
		contact.getUserId() == INFOMANAGER->getUserAccount().getId())
	{
		nameFont.setBold(true);
	}
	
	if (selected || (mouseOver && ((NewContactListWidget2*)parent())->dragged()))
		painter->setPen(Qt::white);
	else
		painter->setPen(QColor(0, 211, 253));

	painter->setFont(nameFont);	
	
	int nameX;
	QRect nameRect;
	
	nameX = 22;
	nameRect.setRect(rect.left() + nameX, rect.top() + 3, rect.width() - nameX - 3, CONTACT_ITEM_HEIGHT - 5);
	bool isCropped = false;
	QFontMetrics fontMetrics(nameFont);
	QString croppedName = UiTools::cropTextByRect(&fontMetrics, nameRect, 
		contact.getDisplayName(), Qt::AlignLeft | Qt::AlignTop, isCropped);
		
	painter->drawText(nameRect, croppedName, QTextOption(Qt::AlignLeft | Qt::AlignTop));

	// number of unread messages
	if (unreadMessages > 0)
	{
		QRect circleRect(rect.width() - 16 - 5, rect.top() + 4, 16, 16);
		painter->setPen(Qt::NoPen);
		painter->setBrush(QColor(255, 0, 0));
		painter->drawEllipse(circleRect);

		QFont unreadFont("Verdana");
		unreadFont.setPixelSize(10);
		unreadFont.setBold(true);

		QString unreadText;
		if (unreadMessages < 100)
			unreadText = QString::number(unreadMessages);
		else
			unreadText = "...";

		QRect textRect;
		if (unreadMessages < 10)
			textRect = QRect(circleRect.left(), circleRect.top() - 1, 15, 16);
		else
			textRect = QRect(circleRect.left(), circleRect.top() - 1, 16, 16);

		painter->setPen(Qt::white);
		painter->setFont(unreadFont);
		painter->drawText(textRect, Qt::AlignCenter, unreadText);
	}
		
	painter->restore();
}

void ContactListItemDelegate::setHoveredIndex(const QModelIndex &index)
{
	const QModelIndex lastIndex = hoveredIndex;

	hoveredIndexSet = true;
	hoveredIndex = QPersistentModelIndex(index);

	if (hoveredIndex == lastIndex)
		return;
	
	if (hoveredIndex.isValid())
		parentList->update(hoveredIndex);

	if (lastIndex.isValid())
		parentList->update(lastIndex);
}

void ContactListItemDelegate::clearHoveredIndex()
{ 
	hoveredIndexSet = false;
	hoveredIndex = QPersistentModelIndex();
}

// ContactListModel class
bool ContactListModel::dropMimeData(const QMimeData *data, Qt::DropAction action, int row, int, const QModelIndex &)
{
	Q_UNUSED(action);
	Q_UNUSED(row);

	return data->hasImage();
}

}