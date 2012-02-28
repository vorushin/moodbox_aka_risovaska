#include "tvwidget.h"

#include <QDir>
#include <QMenu>
#include <QAction>
#include <QApplication>
#include <QClipboard>
#include <QMessageBox>

#include "messagemanager.h"
#include "peopleinfomanager.h"
#include "messageorganizer.h"
#include "messagefile.h"
#include "international.h"
#include "uitools.h"
#include "contactinfodialog.h"
#include "imagetools.h"

#include "debug.h"
#include "testtools.h"

namespace MoodBox 
{

// ObsceneChannelMessageRequest class
ObsceneChannelMessageRequest::ObsceneChannelMessageRequest(TVWidget *parent, qint32 channelId, const MessageKey &key)
 : ServerRequest()
{
	this->key = key;
	this->messageType = parent->getMessageType();
	this->recipientId = parent->getRecipient();

	SERVER->obsceneChannelMessage(CALLBACK(this, onObsceneChannelMessageRequestResult, StandartResultCode::StandartResultCodeEnum), channelId, key.getId());
}

void ObsceneChannelMessageRequest::onObsceneChannelMessageRequestResult(QVariant state, Fault fault, StandartResultCode::StandartResultCodeEnum result)
{
	Q_UNUSED(state)
	Q_UNUSED(result)

	if (fault.isNull())
	{
		MESSAGEMANAGER->deleteMessage(key, messageType, recipientId);
	}

	deleteLater();
}

// DeleteMessageRequest class
DeleteMessageRequest::DeleteMessageRequest(TVWidget *parent, qint32 channelId, const MessageKey &key)
 : ServerRequest()
{
	this->key = key;
	this->messageType = parent->getMessageType();
	this->recipientId = parent->getRecipient();

	SERVER->deleteMessage(CALLBACK(this, onDeleteMessageRequestResult, StandartResultCode::StandartResultCodeEnum), channelId, MessageType::Channel, key.getId());
}

void DeleteMessageRequest::onDeleteMessageRequestResult(QVariant state, Fault fault, StandartResultCode::StandartResultCodeEnum result)
{
	Q_UNUSED(state)
	Q_UNUSED(result)

	if (fault.isNull())
	{
		switch (result)
		{
			case StandartResultCode::Forbidden:
				UiTools::showDialog(NULL, tr(YOU_WAS_FORBIDDEN_TO_DELETE_TITLE), tr(YOU_WAS_FORBIDDEN_TO_DELETE_DESCRIPTION), QMessageBox::Ok);
				break;
			default:
				MESSAGEMANAGER->deleteMessage(key, messageType, recipientId);
				break;
		}
	}

	deleteLater();
}

// Class TVWidget
TVWidget::TVWidget(QWidget *parent)
	: QWidget(parent), showingHistory(false), frozen(false), populating(false), 
	  obsceneButtonAvailable(false), deleteMessageButtonAvailable(false), specialButtonsShowable(false),
	  displayingAuthorId(-1)
{
	TimeMeasure t("TVWidget");

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	// Freeze timer
	freezeTimer.setSingleShot(true);
	freezeTimer.setInterval(TV_FREEZE_TIMER_DURATION);

	connect(&freezeTimer, SIGNAL(timeout()), this, SLOT(onFreezeTimerExpired()));

	obsceneButton->hide();
	deleteMessageButton->hide();

	// Context menu
	createContextMenu();

	// Account state
	connect(INFOMANAGER, SIGNAL(contactListChanged()), this, SLOT(onContactListChanged()));
	connect(INFOMANAGER, SIGNAL(contactNameChanged(qint32, const QString &)), this, SLOT(onContactNameChanged(qint32, const QString &)));
	
	// Connect message notifications
	connect(MESSAGEMANAGER, SIGNAL(privateMessageSent(qint32, const MessageKey &)), this, SLOT(onPrivateMessageSent(qint32, const MessageKey&)));
	connect(MESSAGEMANAGER, SIGNAL(friendsMessageSent(const MessageKey &)), this, SLOT(onFriendsMessageSent(const MessageKey &)));

	connect(MESSAGEMANAGER, SIGNAL(privateMessageSentUpdate(qint32, const MessageKey &, ContactResultCode::ContactResultCodeEnum, const MessageKey &)),
			this, SLOT(onPrivateMessageSentUpdate(qint32, const MessageKey &, ContactResultCode::ContactResultCodeEnum, const MessageKey &)));
	connect(MESSAGEMANAGER, SIGNAL(friendsMessageSentUpdate(const MessageKey &, ContactResultCode::ContactResultCodeEnum, const MessageKey &)),
			this, SLOT(onFriendsMessageSentUpdate(const MessageKey &, ContactResultCode::ContactResultCodeEnum, const MessageKey &)));

	connect(MESSAGEMANAGER, SIGNAL(privateMessageReceived(qint32, const MessageKey &)), this, SLOT(onPrivateMessageReceived(qint32, const MessageKey &)));
	connect(MESSAGEMANAGER, SIGNAL(channelMessageReceived(qint32, const MessageKey &)), this, SLOT(onPrivateMessageReceived(qint32, const MessageKey &)));
	connect(MESSAGEMANAGER, SIGNAL(friendsMessageReceived(const MessageKey &)), this, SLOT(onFriendsMessageReceived(const MessageKey &)));

	connect(MESSAGEMANAGER, SIGNAL(messageDeleted(const MessageKey &)), this, SLOT(messageDeleted(const MessageKey &)));

	// Preview label
	connect(previewLabel, SIGNAL(mouseInPreview()), this, SLOT(onMouseInPreview()));
	connect(previewLabel, SIGNAL(mouseOutPreview()), this, SLOT(onMouseOutPreview()));
}

void TVWidget::setRecipient(MessageType::MessageTypeEnum type, qint32 recipientId, ContactType::ContactTypeEnum currentContactType)
{
	if (getMessageType() == type && getRecipient() == recipientId && isMessageTypeInitialized())
		return;

	isModerator = INFOMANAGER->getUserAccount().getModerateContacts().contains(recipientId) ||
	    		  INFOMANAGER->getUserAccount().getRole() == Role::Admin;

	this->currentContactType = currentContactType;

	MessageTypeMix::setRecipient(type, recipientId);

	reload();
}

void TVWidget::reload()
{
	stopTimer();
	clearMessages();

	if (!INFOMANAGER->getIsLoggedOn())
		return;

	QDir messagesFolder;

	switch (getMessageType())
	{
		case MessageType::Private: 
			if (recipientId < 0)
				return;

			messagesFolder = MessageOrganizer::getContactHistoryFolder(recipientId);
			break;

		case MessageType::Friends:
			messagesFolder = MessageOrganizer::getFriendsHistoryFolder();
			break;

		default:
			Q_ASSERT_X(false, "TVWidget::reload", "Not supported message type");
	}

	TimeMeasure t;

	QStringList filter;
	filter << QString("*") + MESSAGE_FILE_EXTENSION;

	QStringList messageFileNames = messagesFolder.entryList(filter, QDir::Files);

	t.showTimePassed("History file names load time");

	populating = true;

	t.resetTime();

	foreach (QString messageFileName, messageFileNames)
	{
		loadMessageFromFile(messagesFolder.absolutePath() + "/" + messageFileName);
	}

	folderCleanup(messagesFolder.absolutePath());

	t.showTimePassed("History load time");

	populating = false;

	scrollTo(End);
	updateScreen(Automatic);

	emit collectionPopulated();
}

void TVWidget::reset()
{
	if (showingHistory)
		stopShowingHistory();

	reload();
	updateScreen(Notification);

	unInitializeMessageType();
}

QString TVWidget::getPreviewFileName(const MessageKey &key)
{
	return MessageOrganizer::getPreviewFileName(key, getMessageType(), getRecipient());
}

TVMessageInfo TVWidget::getMessageInfo(const MessageKey &key)
{
	return TVMessageInfo::getMessageInfo(key, getMessageType(), getRecipient());
}

void TVWidget::previousMessage()
{
	scrollTo(Previous);
	updateScreen(Manual);

	updateTimer();
}

void TVWidget::nextMessage()
{
	scrollTo(Next);
	updateScreen(Manual);

	updateTimer();
}

void TVWidget::scrollToMessage(const MessageKey &key)
{
	if (!messages.contains(key))
		return;

	stopShowingHistory();

	setCurrentMessage(key);

	updateScreen(Manual);

	updateTimer();
}

void TVWidget::showHistoryMessage(const MessageKey &key)
{
	if (!messages.contains(key))
		return;

	showingHistory = true;
	previewMessageKey = key;

	updateScreen(Manual);
}

void TVWidget::stopShowingHistory()
{
	if (!showingHistory)
		return;

	showingHistory = false;

	updateScreen(Manual);
}

void TVWidget::replyChanged()
{
	showReplyControls();
}

void TVWidget::messageDeleted(const MessageKey &key)
{
	if (messages.contains(key))
	{
		bool wasCurrent = currentMessageKey == key;

		removeMessage(key);

		if (wasCurrent)
			updateScreen(Notification);
	}
}

void TVWidget::deleteMessage(const MessageKey &key)
{
	QList <MessageKey> keys;

	keys << key;

	deleteMessages(keys);
}

void TVWidget::deleteMessages(const QList <MessageKey> &keys)
{
	if (QMessageBox::Cancel == QMessageBox::question(this, tr(TV_DELETE_TITLE), tr(TV_DELETE_TEXT), QMessageBox::Ok | QMessageBox::Cancel))
		return;

	foreach (MessageKey key, keys)
	{
		if (messages.contains(key))
			if (!MESSAGEMANAGER->deleteMessage(key, getMessageType(), getRecipient()))
			{
				QMessageBox::warning(this, tr(TV_DELETE_TITLE), tr(TV_DELETE_ERROR_TEXT));
			}
	}
}

void TVWidget::saveMessage()
{
	if (previewLabel->pixmap() == NULL)
		return;

	const QPixmap pixmap = *(previewLabel->pixmap());

	if (pixmap.isNull())
		return;

	ImageTools::saveImageToFile(pixmap.toImage(), this);
}

void TVWidget::copyMessage()
{
	if (previewLabel->pixmap() == NULL)
		return;

	const QPixmap pixmap = *(previewLabel->pixmap());

	if (pixmap.isNull())
		return;

	QApplication::clipboard()->setPixmap(pixmap);
}

void TVWidget::createContextMenu()
{
	QMenu *contextMenu = new QMenu(this);

	deleteAction = new QAction(tr(TVMENU_DELETE_ACTION_TEXT), this);
	connect(deleteAction, SIGNAL(triggered()), this, SLOT(onDeleteMessage()));
	contextMenu->addAction(deleteAction);

	saveAsAction = new QAction(tr(TVMENU_SAVE_ACTION_TEXT), this);
	connect(saveAsAction, SIGNAL(triggered()), this, SLOT(saveMessage()));
	contextMenu->addAction(saveAsAction);

	copyAction = new QAction(tr(TVMENU_COPY_ACTION_TEXT), this);
	connect(copyAction, SIGNAL(triggered()), this, SLOT(copyMessage()));
	contextMenu->addAction(copyAction);

	publishAction = new QAction(tr(TVMENU_PUBLISH_ACTION_TEXT), this);
	connect(publishAction, SIGNAL(triggered()), this, SLOT(onPublishMessage()));
	contextMenu->addAction(publishAction);

	contextMenu->addSeparator();

	deleteMessageAction = new QAction(tr(TVMENU_DELETE_MESSAGE_ACTION_TEXT), this);
	connect(deleteMessageAction, SIGNAL(triggered()), this, SLOT(on_deleteMessageButton_clicked()));
	contextMenu->addAction(deleteMessageAction);

	obsceneAction = new QAction(tr(TVMENU_OBSCENE_ACTION_TEXT), this);
	connect(obsceneAction, SIGNAL(triggered()), this, SLOT(on_obsceneButton_clicked()));
	contextMenu->addAction(obsceneAction);
	
	previewLabel->setContextMenu(contextMenu);
}

void TVWidget::clearMessages()
{
	clearCurrentMessage();

	messages.clear();

	changedMessagesCollection();

	emit collectionCleared();
}

void TVWidget::clearScreen()
{	
	qint32 emptyId = -1;
	QString emptyLogin;

	if (getMessageType() == MessageType::Private)
	{
		emptyId = getRecipient();
		emptyLogin = INFOMANAGER->getPersonName(emptyId);
	}

	updateAuthorName(emptyId, emptyLogin);

	updateSentDate(QDateTime());
	updateSentStatus(NA);
	updateObsceneButtonAvailability(-1);
	updateDeleteMessageButtonAvailability(-1);
	hideReplyControls();

	// TODO: show "Empty" pixmap
	previewLabel->setPixmap(QPixmap());
}

void TVWidget::folderCleanup(const QString &folderPath)
{
	if (currentContactType != ContactType::Channel && getMessageType() != MessageType::Friends)
		return;

	if (messages.count() <= TV_CHANNEL_FOLDER_CLEANUP_MAX_MESSAGES)
		return;

	while (messages.size() > TV_CHANNEL_FOLDER_CLEANUP_MAX_MESSAGES)
	{
		MessageKey key = messages.keys().first();
		QString fileName = folderPath + "/" + MessageOrganizer::getFileName(key);
		
		MessageOrganizer::removeFiles(fileName);

		messages.remove(key);
	}
}

void TVWidget::loadMessageFromFile(const QString &fileName)
{
	MessageKey key = MessageOrganizer::getKey(fileName);

	bool added = !messages.contains(key);
		
	messages[key] = 0;

	if (added && !populating)
		emit collectionItemAdded(key);

	changedMessagesCollection();
}

void TVWidget::scrollTo(ScrollDirection destination)
{
	if (messages.isEmpty())
		return;

	MessageKey newKey;

	if (!currentMessageKey.isValid())
	{
		if (destination == Beginning || destination == Previous)
			newKey = messages.keys().first();
		else
			newKey = messages.keys().last();
	}
	else
	{
		QMap <MessageKey, int>::iterator current;
		current = messages.find(currentMessageKey);

		switch (destination)
		{
			case Beginning: newKey = messages.keys().first();
				break;

			case Previous: 
				newKey = (currentMessageKey == messages.keys().first()) ? currentMessageKey : (--current).key();
				break;

			case Next: 
				newKey = (currentMessageKey == messages.keys().last()) ? currentMessageKey : (++current).key();
				break;

			case End: newKey = (messages.keys().last());
				break;
		}
	}
		
	setCurrentMessage(newKey);
}

void TVWidget::setCurrentMessage(const MessageKey &key)
{
	if (key == currentMessageKey)
		return;

	if (key.isValid() && !messages.contains(key))
	{
		Q_ASSERT_X(false, "TVWidget::setCurrentMessage", "Unknown key");
		return;
	}

	currentMessageKey = key;

	// Uncomment for debug
	// QDEBUG("TVWidget:: current message key" << currentMessageKey.getDate().toString());

	changedCurrentMessage();
}

void TVWidget::clearCurrentMessage()
{
	setCurrentMessage(MessageKey(QDateTime(), -1));
}

void TVWidget::removeMessage(const MessageKey &key)
{
	if (currentMessageKey == key)
	{
		if (messages.count() == 1)
			clearCurrentMessage();
		else
		{
			if (messages.keys().last() != key)
				scrollTo(Next);
			else
				scrollTo(Previous);
		}
	}

	messages.remove(key);

	emit collectionItemRemoved(key);

	changedMessagesCollection();
}

void TVWidget::messageAdded(const MessageKey &key)
{
	if (key == currentMessageKey)
	{
		// Force update of displaying image
		updateScreen(Notification);
		return;
	}

	if (frozen)
		return;

	// Not current - we do not care
	if (currentMessageKey.isValid())
	{
		if (key < currentMessageKey)
			return;
	}

	setCurrentMessage(key);
	updateScreen(Automatic);
}

void TVWidget::changedCurrentMessage()
{
	updateActions();
}

void TVWidget::changedMessagesCollection()
{
	updateActions();
}

void TVWidget::updateScreen(ActionSource source)
{
	if (showingHistory)
	{
		updateMessage(previewMessageKey);		
	}
	else
	{
		// No display in case of program events when frozen
		if (frozen && source == Automatic)
			return;

		updateMessage(currentMessageKey);
	}
	
	updateActions();
}

void TVWidget::updateMessage(const MessageKey &key)
{
	if (messages.contains(key))
	{
		TVMessageInfo messageInfo = TVMessageInfo::getMessageInfo(key, getMessageType(), getRecipient());

		if (messageInfo.isNull)
		{
			Q_ASSERT_X(false, "TVWidget::updateMessage", "got null messageInfo");

			clearScreen();
			return;
		}

		displayingAuthorId = messageInfo.authorId;
		displayingAuthorLogin = messageInfo.authorLogin;

		updateAuthorName(messageInfo.authorId, messageInfo.authorLogin);
		updateSentDate(key.getDate());
		updateSentStatus(messageInfo.sent ? Sent : NotSent);
		updateObsceneButtonAvailability(messageInfo.authorId);
		updateDeleteMessageButtonAvailability(messageInfo.authorId);

		QPixmap preview(messageInfo.previewFileName);

		if (preview.isNull())
			hideReplyControls();
		else
			showReplyControls();

		// TODO: show "Loading" pixmap
		previewLabel->setPixmap(preview);
	}
	else
	{
		clearScreen();
	}
}

void TVWidget::updateAuthorName(qint32 authorId, const QString authorLogin)
{
	QString authorName;

	if (INFOMANAGER->getIsLoggedOn())
	{
		if (authorId == INFOMANAGER->getUserAccount().getId())
		{
			if (!messages.isEmpty())
				authorName = INFOMANAGER->getUserAccount().getDisplayName();
		}
		else
		{
			if (currentContactType == ContactType::Channel)
				authorId = INFOMANAGER->getContactId(authorLogin);
			
			if (authorId < 0)
			{
				if (!messages.isEmpty())
					authorName = authorLogin;
			}
			else
			{
				ContactInfo *contactInfo = INFOMANAGER->getContact(authorId);

				if (contactInfo != NULL && !messages.isEmpty())
					authorName = INFOMANAGER->getContact(authorId)->getDisplayName();
				else
				{
					// If no contact found may be we have no messages or no contact in list
					if (messages.isEmpty())
						authorName = QString();
					else
					{
						authorName = (!authorLogin.isEmpty()) ? authorLogin : tr(UNKNOWN_AUTHOR);
					}
				}
			}
		}
	}
	userNameButton->setText(authorName);
	userNameButton->setToolTip(authorName);
}

void TVWidget::updateSentDate(const QDateTime &sentDate)
{
	QString sentText = (sentDate.isValid())? 
		(QDate::currentDate() == sentDate.toLocalTime().date()) ? sentDate.toLocalTime().time().toString() : sentDate.toLocalTime().date().toString(Qt::SystemLocaleShortDate) : QString();
	messageDateLabel->setText(sentText);
	messageDateLabel->setToolTip((sentDate.isValid()) ? sentDate.toLocalTime().toString(Qt::SystemLocaleShortDate) : QString());
}

void TVWidget::updateSentStatus(SentStatus sentStatus)
{
	QString text;
	QString hint;

	switch (sentStatus)
	{
		case NotSent: 
			text = tr(NOT_SENT_TEXT);
			hint = tr(NOT_SENT_HINT);
			break;
	}

	sentLabel->setText(text);
	sentLabel->setToolTip(hint);
}

void TVWidget::updateActions()
{
	// Back/forward buttons
	bool previousEnabled = false;
	bool nextEnabled = false;

	MessageKey usingMessageKey;

	if (showingHistory)
	{
		if (!messages.isEmpty())
		{
			previousEnabled = previewMessageKey != messages.keys().first();
			nextEnabled = previewMessageKey != messages.keys().last();

			usingMessageKey = previewMessageKey;
		}
	}
	else
		if (!messages.isEmpty() && currentMessageKey.isValid())
		{
			previousEnabled = currentMessageKey != messages.keys().first();
			nextEnabled = currentMessageKey != messages.keys().last();
			
			usingMessageKey = currentMessageKey;
		}

	emit previousMessageAvailable(previousEnabled);
	emit nextMessageAvailable(nextEnabled);

	// Context menu actions
	const bool deleteEnabled = usingMessageKey.isValid();
	const bool saveCopyPublishEnabled = (previewLabel->pixmap() != NULL && !previewLabel->pixmap()->isNull());

	deleteAction->setEnabled(deleteEnabled);
	saveAsAction->setEnabled(saveCopyPublishEnabled);
	copyAction->setEnabled(saveCopyPublishEnabled);
	publishAction->setEnabled(saveCopyPublishEnabled);
}

void TVWidget::updateObsceneButtonAvailability(qint32 authorId)
{	
	bool available = isModerator && (authorId != -1) && !messages.isEmpty() && (currentContactType == ContactType::Channel) && 
					 INFOMANAGER->getIsLoggedOn() && (authorId != INFOMANAGER->getUserAccount().getId());

	if (obsceneButtonAvailable == available)
		return;

	obsceneButtonAvailable = available;

	updateSpecialButtons();
}

void TVWidget::updateDeleteMessageButtonAvailability(qint32 authorId)
{	
	bool available = isModerator && (authorId != -1) && !messages.isEmpty() && (currentContactType == ContactType::Channel) && 
					 INFOMANAGER->getIsLoggedOn();

	if (deleteMessageButtonAvailable == available)
		return;

	deleteMessageButtonAvailable = available;

	updateSpecialButtons();
}

void TVWidget::setSpecialButtonsShowability(bool showable)
{
	if (specialButtonsShowable == showable)
		return;

	specialButtonsShowable = showable;

	updateSpecialButtons();
}

void TVWidget::updateSpecialButtons()
{
	obsceneAction->setVisible(obsceneButtonAvailable);
	deleteMessageAction->setVisible(deleteMessageButtonAvailable);
	obsceneButton->setVisible(obsceneButtonAvailable && specialButtonsShowable);
	deleteMessageButton->setVisible(deleteMessageButtonAvailable && specialButtonsShowable);
}

void TVWidget::updateTimer()
{
	if (messages.isEmpty() || !currentMessageKey.isValid())
		return;

	if (currentMessageKey == messages.keys().last())
	{
		stopTimer();
		return;
	}

	freezeTimer.start();
	frozen = true;
}

void TVWidget::stopTimer()
{
	if (frozen)
	{
		freezeTimer.stop();
		frozen = false;
	}
}

void TVWidget::showReplyControls()
{
	replyRightArrowButton->show();
}

void TVWidget::hideReplyControls()
{
	replyRightArrowButton->hide();
}

void TVWidget::onContactListChanged()
{
	updateAuthorName(displayingAuthorId, displayingAuthorLogin);
}

void TVWidget::onContactNameChanged(qint32 id, const QString &name)
{
	Q_UNUSED(name)

	if (id == displayingAuthorId)
		updateAuthorName(displayingAuthorId, displayingAuthorLogin);
}

void TVWidget::onPrivateMessageSent(qint32 recipientId, const MessageKey &key)
{
	if (getMessageType() != MessageType::Private || this->recipientId != recipientId)
		return;

	QString fileName = MessageOrganizer::getPrivateMessageFileName(recipientId, MessageOrganizer::getFileName(key));
	
	loadMessageFromFile(fileName);

	messageAdded(key);
}

void TVWidget::onFriendsMessageSent(const MessageKey &key)
{
	if (getMessageType() != MessageType::Friends)
		return;

	QString fileName = MessageOrganizer::getFriendsMessageFileName(MessageOrganizer::getFileName(key));
	
	loadMessageFromFile(fileName);

	messageAdded(key);
}

void TVWidget::onPrivateMessageSentUpdate(qint32 recipientId, const MessageKey &newKey, ContactResultCode::ContactResultCodeEnum result, const MessageKey &oldKey)
{
	Q_UNUSED(result)

	removeMessage(oldKey);

	onPrivateMessageSent(recipientId, newKey);
}

void TVWidget::onFriendsMessageSentUpdate(const MessageKey &newKey, ContactResultCode::ContactResultCodeEnum result, const MessageKey &oldKey)
{
	Q_UNUSED(result)

	removeMessage(oldKey);

	onFriendsMessageSent(newKey);
}

void TVWidget::onPrivateMessageReceived(qint32 authorId, const MessageKey &key)
{
	if (getMessageType() != MessageType::Private || this->recipientId != authorId)
		return;

	QString fileName = MessageOrganizer::getPrivateMessageFileName(authorId, MessageOrganizer::getFileName(key));
	
	loadMessageFromFile(fileName);

	messageAdded(key);
}

void TVWidget::onFriendsMessageReceived(const MessageKey &key)
{
	if (getMessageType() != MessageType::Friends)
		return;

	QString fileName = MessageOrganizer::getFriendsMessageFileName(MessageOrganizer::getFileName(key));
	
	loadMessageFromFile(fileName);

	messageAdded(key);
}

void TVWidget::onFreezeTimerExpired()
{
	stopTimer();

	scrollTo(End);

	updateScreen(Automatic);
}

void TVWidget::onMouseInPreview()
{
	setSpecialButtonsShowability(true);
}

void TVWidget::onMouseOutPreview()
{	
	QPoint obsceneCursorPos = obsceneButton->mapFromGlobal(QCursor::pos());
	QPoint deleteCursorPos = deleteMessageButton->mapFromGlobal(QCursor::pos());

	// If we leave for obscene button it is okay
	if (!obsceneButton->rect().contains(obsceneCursorPos) && !deleteMessageButton->rect().contains(deleteCursorPos))
		setSpecialButtonsShowability(false);
}

void TVWidget::on_replyRightArrowButton_clicked()
{
	QPixmap currentPixmap = *(previewLabel->pixmap());

	if (currentPixmap.isNull())
		return;

	hideReplyControls();

	emit replyRequest(currentPixmap.toImage());
}

void TVWidget::on_obsceneButton_clicked()
{
	new ObsceneChannelMessageRequest(this, getRecipient(), currentMessageKey);
	obsceneButton->hide();
}

void TVWidget::on_deleteMessageButton_clicked()
{
	new DeleteMessageRequest(this, getRecipient(), currentMessageKey);
	deleteMessageButton->hide();
}

void TVWidget::on_userNameButton_clicked()
{
	ContactInfoDialog *contactInfoDialog;
	int contactId;

	if (displayingAuthorId == INFOMANAGER->getUserAccount().getId())
	{
		ContactInfo contact = ContactInfo(displayingAuthorId, INFOMANAGER->getUserAccount().getLogin(), 
			MoodBox::UserStatus::Online, INFOMANAGER->getUserAccount().getMotto(), 
			INFOMANAGER->getUserAccount().getDisplayName(), INFOMANAGER->getUserAccount().getBirthDay(),
			MoodBox::AuthorizationState::Authorized, QString(), false, ContactType::Friend);
		contactInfoDialog = new ContactInfoDialog(contact, this);
	}
	else
	{
		contactId = INFOMANAGER->getContactId(displayingAuthorLogin);
		if (contactId > 0)
		{
			ContactInfo contact;
			contact = *(INFOMANAGER->getContact(contactId));
			if (contact.getUserId() > 0)
			{
				contactInfoDialog = new ContactInfoDialog(contact, this);
			}
			else
				return;
		}
		else
		{
			contactInfoDialog = new ContactInfoDialog(displayingAuthorLogin, this);
		}
	}
	contactInfoDialog->show();
}

void TVWidget::onPublishMessage()
{
	MessageKey key = (showingHistory) ? previewMessageKey : currentMessageKey;

	if (!key.isValid())
		return;

	emit publishRequest(key);
}

void TVWidget::onDeleteMessage()
{
	MessageKey key = (showingHistory) ? previewMessageKey : currentMessageKey;

	deleteMessage(key);
}

};
