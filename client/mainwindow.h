#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>
#include <QSystemTrayIcon>
#include <QHash>
#include <QMenu>

#include "ui_mainwindow.h"

#include "serverresponsehandler.h"
#include "userstatus.h"
#include "autoupdater.h"
#include "messagekey.h"

#include "qtsingleapplication.h"

class QNetworkAccessManager;
class QNetworkReply;
class QProgressDialog;
class QNetworkReply;

namespace MoodBox
{

#define ALL_FRIENDS_CHAT_LABEL			QT_TRANSLATE_NOOP("MoodBox::MainWindow", "AllFriendsLabel")
#define SANDBOX_CHAT_LABEL				QT_TRANSLATE_NOOP("MoodBox::MainWindow", "SandBoxChatLabel")
#define PRIVATE_CHAT_LABEL				QT_TRANSLATE_NOOP("MoodBox::MainWindow", "PrivateChat")

#define DISCONNECT_TITLE				QT_TRANSLATE_NOOP("MoodBox::MainWindow", "DisconnectedLoginInUseTitle")
#define DISCONNECT_TEXT					QT_TRANSLATE_NOOP("MoodBox::MainWindow", "DisconnectedLoginInUseText")

#define NOT_SUPPORTED_TITLE				QT_TRANSLATE_NOOP("MoodBox::MainWindow", "VersionTooOldNeedUpdateTitle")
#define NOT_SUPPORTED_TEXT				QT_TRANSLATE_NOOP("MoodBox::MainWindow", "VersionTooOldNeedUpdateText")

#define REGISTERED_INFO_WIDGET_TITLE	QT_TRANSLATE_NOOP("MoodBox::MainWindow", "RegistrationCompletedTitle")
#define REGISTERED_INFO_WIDGET_TEXT		QT_TRANSLATE_NOOP("MoodBox::MainWindow", "RegistrationCompletedText%1")

#define PASSWORD_INFO_WIDGET_TITLE		QT_TRANSLATE_NOOP("MoodBox::MainWindow", "PasswordSentTitle")
#define PASSWORD_INFO_WIDGET_TEXT		QT_TRANSLATE_NOOP("MoodBox::MainWindow", "PasswordSentText")

#define ONLINE_MENU_ITEM				QT_TRANSLATE_NOOP("MoodBox::MainWindow", "OnlineMenuItem")
#define OFFLINE_MENU_ITEM				QT_TRANSLATE_NOOP("MoodBox::MainWindow", "OfflineMenuItem")
#define SIGNOUT_MENU_ITEM				QT_TRANSLATE_NOOP("MoodBox::MainWindow", "LogoutMenuItem")
#define EXIT_MENU_ITEM					QT_TRANSLATE_NOOP("MoodBox::MainWindow", "ExitMenuItem")
#define NETWORK_SETTINGS_MENU_ITEM		QT_TRANSLATE_NOOP("MoodBox::MainWindow", "NetworkSettingsMenuItem")
#define OPEN_TV_MENU_ITEM				QT_TRANSLATE_NOOP("MoodBox::MainWindow", "OpenMoodbox")

#define YOU_WAS_BANNED_TITLE			QT_TRANSLATE_NOOP("MoodBox::MainWindow", "YouWasBannedTitle")
#define YOU_WAS_BANNED_DESCRIPTION		QT_TRANSLATE_NOOP("MoodBox::MainWindow", "YouWasBannedDescription")

#define CHANNEL_WAS_CLOSED_TITLE		QT_TRANSLATE_NOOP("MoodBox::MainWindow", "ChannelWasClosedTitle")
#define CHANNEL_WAS_CLOSED_DESCRIPTION	QT_TRANSLATE_NOOP("MoodBox::MainWindow", "ChannelWasClosedDescription")

#define DOWLOADING_PROGRESS_MESSAGE		QT_TRANSLATE_NOOP("MoodBox::MainWindow", "DownloadingProgressMessage")
#define DOWNLOAD_INSTALLER_ERROR_TITLE	QT_TRANSLATE_NOOP("MoodBox::MainWindow", "DownloadInstallerErrorTitle")
#define DOWNLOAD_INSTALLER_ERROR_TEXT	QT_TRANSLATE_NOOP("MoodBox::MainWindow", "DownloadInstallerErrorText")

// Position/state saving options
#define MAIN_WINDOW_GROUP						"Main Window"
#define POSITION_OPTION							"Position"
#define SIZE_OPTION								"Size"
#define STATE_OPTION							"State"
#define PRIVATE_CHATS_GROUP						"Private Chats"
#define DRAWING_WINDOW_UNDOCKED_STATE_OPTION	"Drawing Window UnDocked State"
#define DRAWING_WINDOW_POSITION_OPTION			"Drawing Window Position"

#define MAINWINDOW_INITIAL_WIDTH		436
#define MAINWINDOW_INITIAL_HEIGHT		372

#define FIRST_MESSAGE					":/MoodBox/Resources/first_message.png"
#define FIRST_MESSAGE_THUMBNAIL			":/MoodBox/Resources/first_message_thumbnail.png"
#define FIRST_MESSAGE_RU				":/MoodBox/Resources/first_message_ru.png"
#define FIRST_MESSAGE_THUMBNAIL_RU		":/MoodBox/Resources/first_message_thumbnail_ru.png"
#define FIRST_MESSAGE_INTERNAL_FORMAT	"<Message Id=\"0\" Type=\"Friends\" SentDate=\"%1\" Author=\"0\" AuthorLogin=\"Moodbox\" Sent=\"Yes\" IsPublic=\"No\"></Message>"

#define ALL_FRIENDS_ID					-1

class NewContactListWindow;
class TVWindow;
class DrawingWindow;
class HistoryWindow;
class StatusIcon;
class ClipartWindow;
class WidgetSlideAnimation;

using namespace Ui;

// Main application window class
class MainWindow : public QMainWindow, public MainWindowClass
{
	Q_OBJECT

public:
	MainWindow(QtSingleApplication &app);

public slots:
	void onUserAccountUpdated();
	void onUserStatusChanged(UserStatus::UserStatusEnum status);

	void onServerError(ServerResponseHandler::ServerError);
	void onShowExceptionDialogSendingMessage(ContactResultCode::ContactResultCodeEnum ResultCode, qint32 id);

	void onLogonStarted();
	void onLogonStopped();
	void onLogonSuccess();

	void showWindow();
	void shutDown();

	void onLogoutCompleted();
	void relogon();
	void offline(bool userChoosenOffline = true);
	bool logout(bool isShuttingDown = false);

	void showSettings();

	void ipcMessage(const QString &message);

protected:
	// Timer for tray menu
	virtual void timerEvent(QTimerEvent *event);

	// Minimize to tray
	virtual void closeEvent(QCloseEvent *event);

	virtual void mousePressEvent(QMouseEvent *event);
	virtual void mouseMoveEvent(QMouseEvent *event);
	virtual void mouseReleaseEvent(QMouseEvent *event);
	
	virtual void resizeEvent(QResizeEvent *event);
	virtual void moveEvent(QMoveEvent *event);

private:
	enum ScreenWidget { Logon = 0, Registration = 1, ForgotPassword = 2, Welcome = 3, TV = 4, Invite = 5, Info = 6, Waiting = 7 };

	qint32 currentRecipient;

	bool isLoggingOn;
	bool isShuttingDown;

	bool isPlaySounds;

	// Show welcome screen after logon
	bool showWelcomeScreen;

	bool isInviteCodeNeed;

	bool isMousePressed;

	bool showClipartWindow;

	// Dragging & position
	QPoint dragPosition;
	QPoint currentPosition;

	QSize currentSize;

	int widgetIndexBeforeWaiting;

	// Login for adding friend via url
	QString friendLoginFromUrl;

	// Windows
	NewContactListWindow *contactList;
	DrawingWindow *drawingWindow;
	HistoryWindow *historyWindow;
	ClipartWindow *clipartWindow;

	// List of private TVs
	QHash <qint32, TVWindow*> privateTVs;

	// Tray
	QSystemTrayIcon *trayIcon;
	QMenu *trayMenu;

	QAction *openTvAction;
	QAction *networkSettingsAction;
	QAction *onlineAction;
	QAction *offlineAction;
	QAction *signOutAction;
	QAction *exitAction;

	int trayClickTimer;
	QPoint trayClickPosition;
	
	int trayConnectingTimer;
	
	// Updater
	AutoUpdater autoUpdater;

	QNetworkAccessManager *http;
	QProgressDialog *progressDialog;
	QNetworkReply *reply;

#ifdef Q_WS_MAC
    QMenuBar    *appMenuBar;
#endif

	// Tray functions
	void setupTray();
	void toggleTrayMenuTimer();
	void stopTrayMenuTimer();
	void stopTrayConnectingTimer();

	// Window functions
	void showWidget(ScreenWidget showWidget);
	
	void resetWindowGeometry();
	void updateWindowTitle();

	// Child windows functions
	void createChildWindows();
	void resetChildWindows();

	void updateDrawingWindow(qint32 id);
	void updateHistoryWindow(qint32 id);

	// Settings
	void saveSettings() const;
	void loadSettings();

	// Application functions
	void showTrayMenu(const QPoint &pos);

	// operations workflow:
	// init -> start <-> stop -> cleanup

	void init(); // inits all that must be inited when in LoggedOn
	void cleanup(); // cleans up all that must be cleaned up when leaving LoggedOn
	void start(); // starts all activities
	void stop(); // stops all activities

	void updateMask();

	void alertWindow();
	void alertWindowIfVisible();
	bool isActiveOrHasActiveChild() const;

	void postAddFriendLogin(const QString &login);

	void startInstallNewVersion();
    
#ifdef Q_WS_MAC
    void createDockMenu();
    void createMainMenu();
    void macInit();
    void macCleanup();
#endif

private slots:
	void addFriendByLogin();

	void forceQuit();

	void onTrayIconActivated(QSystemTrayIcon::ActivationReason reason);

	void on_contactListButton_toggled(bool checked);
	void on_drawButton_toggled(bool checked);
	void on_historyButton_toggled(bool checked);

	void onDownloadNewVersion();
	void onInviteCodeAccepted();
	void onRegistrationSuccess();
	void onResetPassword();
	void onShowClipart();
	void onReply(const QImage &image);

	void showTvWidget();
	void showLogonWidget(bool updateFields = false);
	void showInviteCodeWidget();
	void showRegistrationWidget();
	void showForgotPasswordWidget();
	void showInfoWidget();
	void showWaitingWidget();
	void stopWaitingWidget();
	void hideWaitingWidget();
	void cancelWaitingWidget();
	void onRegistrationBack();
	void onInviteCodeNeeded();

	void onContactSelected(qint32 id);
	void onContactImageDrop(qint32 id, const QImage &image);

	void onSoundStateChanged(bool enableSounds);

	void onPrivateMessageReceived(qint32 authorId, const MessageKey &key);
	void onChannelMessageReceived(qint32 channelId, const MessageKey &key);
	void onFriendsMessageReceived(const MessageKey &);

	void onMessageSent();

	void showHelp();

	void requestFinished(QNetworkReply *reply);
	void dataProcessedProgress(qint64 done, qint64 total);
	void cancelHttpRequest();
};

}

#endif // MAINWINDOW_H
