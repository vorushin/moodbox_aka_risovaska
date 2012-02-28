#ifndef TVWIDGET_H
#define TVWIDGET_H

#include <QWidget>
#include <QMap>
#include <QTimer>

#include "ui_tvwidget.h"
#include "messagetypemix.h"
#include "serverrequest.h"

#include "tvmessageinfo.h"

#include "standartresultcode.h"
#include "contactresultcode.h"
#include "contacttype.h"
#include "fault.h"

class QAction;
class QMenu;

namespace MoodBox 
{

#define TV_FREEZE_TIMER_DURATION					90000
#define TV_CHANNEL_FOLDER_CLEANUP_MAX_MESSAGES		1000

#define NOT_SENT_TEXT								QT_TRANSLATE_NOOP("MoodBox::TVWidget", "NotSentText")
#define NOT_SENT_HINT								QT_TRANSLATE_NOOP("MoodBox::TVWidget", "NotSentHint")
#define UNKNOWN_AUTHOR								QT_TRANSLATE_NOOP("MoodBox::TVWidget", "Unknown")

#define TVMENU_DELETE_ACTION_TEXT					QT_TRANSLATE_NOOP("MoodBox::TVWidget", "DeleteMessage")
#define TVMENU_SAVE_ACTION_TEXT						QT_TRANSLATE_NOOP("MoodBox::TVWidget", "SaveMessage")
#define TVMENU_COPY_ACTION_TEXT						QT_TRANSLATE_NOOP("MoodBox::TVWidget", "CopyMessage")
#define TVMENU_PUBLISH_ACTION_TEXT					QT_TRANSLATE_NOOP("MoodBox::TVWidget", "PublishMessage")
#define TVMENU_DELETE_MESSAGE_ACTION_TEXT			QT_TRANSLATE_NOOP("MoodBox::TVWidget", "DeleteServerMessage")
#define TVMENU_OBSCENE_ACTION_TEXT					QT_TRANSLATE_NOOP("MoodBox::TVWidget", "ObsceneMessage")

#define TV_DELETE_TITLE								QT_TRANSLATE_NOOP("MoodBox::TVWidget", "DeleteTitle")
#define TV_DELETE_TEXT								QT_TRANSLATE_NOOP("MoodBox::TVWidget", "DeleteText")
#define TV_DELETE_ERROR_TEXT						QT_TRANSLATE_NOOP("MoodBox::TVWidget", "DeleteErrorText")

#define YOU_WAS_FORBIDDEN_TO_DELETE_TITLE			QT_TRANSLATE_NOOP("MoodBox::TVWidget", "YouWasForbiddenToDeleteTitle")
#define YOU_WAS_FORBIDDEN_TO_DELETE_DESCRIPTION		QT_TRANSLATE_NOOP("MoodBox::TVWidget", "YouWasForbiddenToDeleteDescription")

class TVWidget;

// Voting for obscene message
class ObsceneChannelMessageRequest : public ServerRequest
{
	Q_OBJECT

public:
	ObsceneChannelMessageRequest(TVWidget *parent, qint32 channelId, const MessageKey &key);

private:
	MessageKey key;
	MessageType::MessageTypeEnum messageType;
	qint32 recipientId;

private slots:
	void onObsceneChannelMessageRequestResult(QVariant state, Fault fault, StandartResultCode::StandartResultCodeEnum result);
};

class DeleteMessageRequest : public ServerRequest
{
	Q_OBJECT

public:
	DeleteMessageRequest(TVWidget *parent, qint32 channelId, const MessageKey &key);

private:
	MessageKey key;
	MessageType::MessageTypeEnum messageType;
	qint32 recipientId;

private slots:
	void onDeleteMessageRequestResult(QVariant state, Fault fault, StandartResultCode::StandartResultCodeEnum result);
};

using namespace Ui;

// TV widget shows messages of contact or friends
class TVWidget : public QWidget, public MessageTypeMix, public TVWidgetClass
{
	Q_OBJECT

public:
	TVWidget(QWidget *parent = NULL);

	void setChatLabel(QString chatName) { chatLabel->setText(chatName); } ;

	virtual void setRecipient(MessageType::MessageTypeEnum type, qint32 recipientId = -1, ContactType::ContactTypeEnum currentContactType = ContactType::Friend);

	const QMap <MessageKey, int> *getMessagesCollection() const { return &messages; };

	// Reload current info
	void reset();

	QString getPreviewFileName(const MessageKey &key);
	TVMessageInfo getMessageInfo(const MessageKey &key);

signals:
	void replyRequest(const QImage &image);
	void contactListRequest();
	void historyRequest();
	void publishRequest(const MessageKey &key);

	void previousMessageAvailable(bool enabled);
	void nextMessageAvailable(bool enabled);

	// Signals for HistoryWindow
	// POSTPONED make a TVMessageInfoProvider interface
	void collectionItemAdded(const MessageKey &key);
	void collectionItemRemoved(const MessageKey &key);

	void collectionPopulated();
	void collectionCleared();

public slots:
	void reload();

	// Scroll to previous message
	virtual void previousMessage();
	// Scroll to next message
	virtual void nextMessage();
	// Scroll to specific message
	virtual void scrollToMessage(const MessageKey &key);
	
	// Preview management
	virtual void showHistoryMessage(const MessageKey &key);
	virtual void stopShowingHistory();

	// Posted reply changed
	virtual void replyChanged();

	// Message deleted
	void messageDeleted(const MessageKey &key);

	// Delete message or messages
	void deleteMessage(const MessageKey &key);
	void deleteMessages(const QList <MessageKey> &keys);

	// Utility operations	
	void saveMessage();
	void copyMessage();

private:
	enum ScrollDirection { Beginning, Previous, Next, End };
	enum ActionSource { Manual, Automatic, Notification };
	enum SentStatus { NotSent, Sent, NA };

	QMap <MessageKey, int> messages;
	MessageKey currentMessageKey, previewMessageKey;
	
	bool showingHistory, frozen, populating;
	bool obsceneButtonAvailable, deleteMessageButtonAvailable, specialButtonsShowable;

	qint32 displayingAuthorId;
	QString displayingAuthorLogin;

	ContactType::ContactTypeEnum currentContactType;

	bool isModerator; // current user is moderator for current contact in Contact List

	QTimer freezeTimer;

	QAction *deleteAction, *saveAsAction, *copyAction, *publishAction, *deleteMessageAction, *obsceneAction;

	// Create context menu
	void createContextMenu();

	// Remove all messages
	void clearMessages();

	// Clear TV screen
	void clearScreen();

	// Clean-up folder
	void folderCleanup(const QString &folderPath);

	// Load single message file
	void loadMessageFromFile(const QString &fileName);

	// Scroll to destination
	void scrollTo(ScrollDirection destination);

	// Current message management
	void setCurrentMessage(const MessageKey &key);
	void clearCurrentMessage();

	// Remove specific message
	void removeMessage(const MessageKey &key);

	// Update considering new message
	void messageAdded(const MessageKey &key);

	// Change events
	void changedCurrentMessage();
	void changedMessagesCollection();	

	// Updates
	void updateScreen(ActionSource source);
	void updateMessage(const MessageKey &key);

	void updateAuthorName(qint32 authorId, const QString authorLogin);
	void updateSentDate(const QDateTime &sentDate);
	void updateSentStatus(SentStatus sentStatus);
	
	void updateActions();

	void updateObsceneButtonAvailability(qint32 authorId);
	void updateDeleteMessageButtonAvailability(qint32 authorId);

	void setSpecialButtonsShowability(bool showable);

	void updateSpecialButtons();

	void updateTimer();
	void stopTimer();

	void showReplyControls();
	void hideReplyControls();

private slots:
	void onContactListChanged();
	void onContactNameChanged(qint32 id, const QString &name);

	void onPrivateMessageSent(qint32 recipientId, const MessageKey &key);
	void onFriendsMessageSent(const MessageKey &key);
	
	void onPrivateMessageSentUpdate(qint32 recipientId, const MessageKey &newKey, ContactResultCode::ContactResultCodeEnum result, const MessageKey &oldKey);
	void onFriendsMessageSentUpdate(const MessageKey &newKey, ContactResultCode::ContactResultCodeEnum result, const MessageKey &oldKey);

	void onPrivateMessageReceived(qint32 authorId, const MessageKey &key);
	void onFriendsMessageReceived(const MessageKey &key);

	void onFreezeTimerExpired();

	void onMouseInPreview();
	void onMouseOutPreview();

	void on_replyRightArrowButton_clicked();
	void on_obsceneButton_clicked();
	void on_deleteMessageButton_clicked();
	void on_userNameButton_clicked();

	void onPublishMessage();
	void onDeleteMessage();
};

}

#endif // TVWIDGET_H
