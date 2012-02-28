#include "mainwindow.h"

#include <QMessageBox>
#include <QCloseEvent>
#include <QSettings>
#include <QDesktopWidget>
#include <QMenu>
#include <QSound>
#include <QShortcut>
#include <QFile>
#include <QMessageBox>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QNetworkRequest>
#include <QDir>
#include <QProgressBar>
#include <QProcess>

#include "peopleinfomanager.h"
#include "messagemanager.h"
#include "messagefile.h"
#include "messagekey.h"
#include "messageorganizer.h"

#include "newcontactlistwindow.h"
#include "authorizationdialog.h"

#include "clipartwindow.h"
#include "drawingwindow.h"
#include "historywindow.h"

#include "uitools.h"
#include "international.h"
#include "debug.h"
#include "common.h"
#include "testtools.h"
#include "language.h"

#include "statusicon.h"

#include "servercontrol.h"

#include "apptools.h"
#include "soundsframe.h"
#include "programsettings.h"
#include "verifiers.h"

#include "channelinfomanager.h"

#ifdef Q_WS_MAC
#include <QMenuBar>
#include "mactools.h"

// vhbit: function is exported by Qt, no include files required
void qt_mac_set_dock_menu(QMenu *menu);
#endif

#ifdef Q_WS_X11
#include "linuxtools.h"
#endif

namespace MoodBox
{

// vhbit: custom window title flag is applied only on window creation,
// so we can not change it later and need pass to constructor
#ifndef Q_WS_WIN
#define MAINWINDOWFLAGS     Qt::CustomizeWindowHint
#else
#define MAINWINDOWFLAGS     0
#endif
    
MainWindow::MainWindow(QtSingleApplication &app)
	: QMainWindow(NULL, MAINWINDOWFLAGS), 
	  isLoggingOn(false), isShuttingDown(false), 
	  showWelcomeScreen(false), isInviteCodeNeed(false), isMousePressed(false), showClipartWindow(false),
	  widgetIndexBeforeWaiting(-1),
	  contactList(NULL), drawingWindow(NULL), historyWindow(NULL), 
	  trayIcon(0), trayMenu(NULL), trayClickTimer(-1), trayConnectingTimer(-1), 	
	  autoUpdater(this), http(NULL), reply(NULL)
{
   // AEInstallEventHandler(
    
	setWindowFlags(windowFlags() & ~Qt::WindowMaximizeButtonHint);

	TimeMeasure t("MainWindow");

	progressDialog = NULL;

	setupUi(this);

	t.showTimePassedAfterSetupUi();

	// connect signals from second copy of the application
	connect(&app, SIGNAL(messageReceived(const QString & )), this, SLOT(showWindow()));

	isPlaySounds = ProgramSettings::getPlaySouns();

	// Connect infomanager & server
	connect(INFOMANAGER, SIGNAL(userAccountUpdated()), this, SLOT(onUserAccountUpdated()));
	connect(INFOMANAGER, SIGNAL(userStatusChanged(UserStatus::UserStatusEnum)), this, SLOT(onUserStatusChanged(UserStatus::UserStatusEnum)));

	connect(SERVER, SIGNAL(serverError(ServerResponseHandler::ServerError)), this, SLOT(onServerError(ServerResponseHandler::ServerError)));
	connect(SERVER, SIGNAL(logoutCompleted()), this, SLOT(onLogoutCompleted()));

	// Connect message manager
	connect(MESSAGEMANAGER, SIGNAL(privateMessageReceived(qint32, const MessageKey &)), this, SLOT(onPrivateMessageReceived(qint32, const MessageKey &)));
	connect(MESSAGEMANAGER, SIGNAL(friendsMessageReceived(const MessageKey &)), this, SLOT(onFriendsMessageReceived(const MessageKey &)));
	connect(MESSAGEMANAGER, SIGNAL(channelMessageReceived(qint32, const MessageKey &)), this, SLOT(onChannelMessageReceived(qint32, const MessageKey &)));
	connect(MESSAGEMANAGER, SIGNAL(showExceptionDialogSendingMessage(ContactResultCode::ContactResultCodeEnum, qint32)), this, SLOT(onShowExceptionDialogSendingMessage(ContactResultCode::ContactResultCodeEnum, qint32)));
		
	// Connect logon
	connect(logonWidget, SIGNAL(registerNewUser()), this, SLOT(showRegistrationWidget()));
	connect(logonWidget, SIGNAL(inviteCodeNeeded()), this, SLOT(onInviteCodeNeeded()));
	connect(logonWidget, SIGNAL(forgotPassword()), this, SLOT(showForgotPasswordWidget()));
	connect(logonWidget, SIGNAL(logonStarted()), this, SLOT(onLogonStarted()));
	connect(logonWidget, SIGNAL(logonStopped()), this, SLOT(onLogonStopped()));
	connect(logonWidget, SIGNAL(successLogon()), this, SLOT(onLogonSuccess()));
	connect(logonWidget, SIGNAL(showSettings()), this, SLOT(showSettings()));

	// connect widgets with waiting widget
	connect(waitingWidget, SIGNAL(cancel()), this, SLOT(cancelWaitingWidget()));
	connect(logonWidget, SIGNAL(waitingStart()), this, SLOT(showWaitingWidget()));
	connect(logonWidget, SIGNAL(waitingStop()), this, SLOT(stopWaitingWidget()));
	connect(registrationWidget, SIGNAL(waitingStart()), this, SLOT(showWaitingWidget()));
	connect(registrationWidget, SIGNAL(waitingHide()), this, SLOT(cancelWaitingWidget()));
	connect(forgotPasswordWidget, SIGNAL(waitingStart()), this, SLOT(showWaitingWidget()));
	connect(forgotPasswordWidget, SIGNAL(waitingStop()), this, SLOT(cancelWaitingWidget()));
	connect(inviteCodeWidget, SIGNAL(waitingStart()), this, SLOT(showWaitingWidget()));
	connect(inviteCodeWidget, SIGNAL(waitingStop()), this, SLOT(cancelWaitingWidget()));

	// Connect invite
	connect(inviteCodeWidget, SIGNAL(next()), this, SLOT(onInviteCodeAccepted()));
	connect(inviteCodeWidget, SIGNAL(back()), this, SLOT(showLogonWidget()));

	// Connect registration
	connect(registrationWidget, SIGNAL(back()), this, SLOT(onRegistrationBack()));
	connect(registrationWidget, SIGNAL(registrationSuccess()), this, SLOT(onRegistrationSuccess()));

	// Connect info
	connect(infoWidget, SIGNAL(finished()), this, SLOT(showLogonWidget()));

	// Connect forgot password
	connect(forgotPasswordWidget, SIGNAL(finished()), this, SLOT(onResetPassword()));
	connect(forgotPasswordWidget, SIGNAL(back()), this, SLOT(showLogonWidget()));

	// Connect welcome
	connect(welcomeWidget, SIGNAL(finished()), this, SLOT(showTvWidget()));

	// Connect buttons
	connect(tvCloseButton, SIGNAL(clicked()), this, SLOT(close()));

	// Connect TVWidget
	connect(previousButton, SIGNAL(clicked()), tvWidget, SLOT(previousMessage()));
	connect(tvWidget, SIGNAL(previousMessageAvailable(bool)), previousButton, SLOT(setEnabled(bool)));
	connect(tvWidget, SIGNAL(previousMessageAvailable(bool)), this, SLOT(showTvWidget()));
	
	connect(nextButton, SIGNAL(clicked()), tvWidget, SLOT(nextMessage()));
	connect(tvWidget, SIGNAL(nextMessageAvailable(bool)), nextButton, SLOT(setEnabled(bool)));
	connect(tvWidget, SIGNAL(nextMessageAvailable(bool)), this, SLOT(showTvWidget()));

	connect(tvWidget, SIGNAL(replyRequest(const QImage &)), this, SLOT(onReply(const QImage &)));

	tvWidget->setChatLabel(tr(ALL_FRIENDS_CHAT_LABEL));

	// Init variables & windows
	autoUpdater.startCheckingPaused();
	connect(&autoUpdater, SIGNAL(downloadNewVersion()), this, SLOT(onDownloadNewVersion()));

	clipartWindow = new ClipartWindow(this);

	createChildWindows();

	loadSettings();

	// start logon
	showLogonWidget(true);
	logonWidget->start(true);

	buttonsFrame->hide();

	setupTray();
	updateWindowTitle();

	// show help by pressing F1
	QShortcut * showHelpShortCut = new QShortcut(this);
	showHelpShortCut->setKey(Qt::Key_F1);
	connect(showHelpShortCut, SIGNAL(activated()), this, SLOT(showHelp()));
    
#ifdef Q_WS_MAC
    macInit();
#endif
}

void MainWindow::onUserAccountUpdated()
{
	updateWindowTitle();
}

void MainWindow::onUserStatusChanged(UserStatus::UserStatusEnum status)
{
	if (status == UserStatus::Connecting) 
	{
		trayConnectingTimer = startTimer(100);
	}
	else
	{
		stopTrayConnectingTimer();
		trayIcon->setIcon(StatusIcon::getStatusIcon(status));
	}
}

void MainWindow::onServerError(ServerResponseHandler::ServerError error) 
{
	QMessageBox *messageDialog;
	QPushButton *installButton;
	QPushButton *cancelButton;
	switch(error)
	{
		case MoodBoxCustomServer::Disconnect:
			Q_ASSERT_X(INFOMANAGER->isUserOnline(), "MainWindow::onServerError", "User is offline, but got Disconnect");
			offline(false);
			QMessageBox::information(this, tr(DISCONNECT_TITLE), tr(DISCONNECT_TEXT));
			break;

		case MoodBoxCustomServer::UnsupportedClientVersion:
			offline(false);
			messageDialog = new QMessageBox(QMessageBox::Information, tr(NOT_SUPPORTED_TITLE), tr(NOT_SUPPORTED_TEXT));
			installButton = messageDialog->addButton(tr(INSTALL_NEW_VERSION_BUTTON), QMessageBox::YesRole);
			cancelButton = messageDialog->addButton(tr(CANCEL_TEXT), QMessageBox::NoRole);
			messageDialog->exec();
			if (messageDialog->clickedButton() == installButton)
			{
				startInstallNewVersion();
			};
			messageDialog->deleteLater();
			break;

		case MoodBoxCustomServer::NotAuthenticated:
			Q_ASSERT_X(INFOMANAGER->isUserOnline() || isLoggingOn, "MainWindow::onServerError", "User is offline and not logging on, but got NotAuthenticated");
			if(INFOMANAGER->isUserOnline() || isLoggingOn)
			{
				relogon();
			}
			break;

		case MoodBoxCustomServer::NotificationServerRegistrationFailed:
			Q_ASSERT_X(INFOMANAGER->isUserOnline() || isLoggingOn, "MainWindow::onServerError", "User is offline and not logging on, but got NotificationServerRegistrationFailed");
			if(INFOMANAGER->isUserOnline() || isLoggingOn)
			{
				relogon();
			}
			break;
	}
}

void MainWindow::onShowExceptionDialogSendingMessage(ContactResultCode::ContactResultCodeEnum ResultCode, qint32 id)
{
	QString name;
	
	switch (ResultCode)
	{
		case ContactResultCode::ClosedContact:
				name = INFOMANAGER->getPersonName(id);
				UiTools::showDialog(NULL, tr(CHANNEL_WAS_CLOSED_TITLE), tr(CHANNEL_WAS_CLOSED_DESCRIPTION).arg(name), QMessageBox::Ok);
				break;

		case ContactResultCode::NotAuthorizedMe:
				name = INFOMANAGER->getPersonName(id);
				UiTools::showDialog(NULL, tr(YOU_WAS_BANNED_TITLE), tr(YOU_WAS_BANNED_DESCRIPTION).arg(name), QMessageBox::Ok);
				break;
	}	
}

void MainWindow::onLogonStarted()
{
	isLoggingOn = true;
	INFOMANAGER->setStatus(UserStatus::Connecting);
}

void MainWindow::onLogonStopped() // NOTE we will not reach here if got StopRequests fault reaction
{
	offline(false);
}

void MainWindow::onLogonSuccess()
{
	hideWaitingWidget();

	// we are: logged on | changed status to online | reconnected after network failure
	LOGONPROVIDER->saveStatus(UserStatus::Online); // TODO need it only for logged on | changed status to online

	if (!INFOMANAGER->getIsLoggedOn())
	{
		init();

		updateWindowTitle();
		buttonsFrame->show();

		showWidget(TV);
	}

	start();

	INFOMANAGER->setStatus(UserStatus::Online);
	isLoggingOn = false;
}

void MainWindow::timerEvent(QTimerEvent *event)
{
	if (event->timerId() == trayClickTimer)
	{
		stopTrayMenuTimer();
		showTrayMenu(trayClickPosition);	
	}
	else if (event->timerId() == trayConnectingTimer)
	{
		QIcon nextConnectingIcon = StatusIcon::getNextConnectingIcon();
		trayIcon->setIcon(nextConnectingIcon);
		if (contactList)
			contactList->setStatusButtonIcon(nextConnectingIcon);
	}

	QObject::timerEvent(event);
}

void MainWindow::closeEvent(QCloseEvent *event)
{
	// Minimize instead of exit
#ifdef Q_WS_MAC
    MacTools::collapseWindows();
#endif
#ifdef Q_WS_X11
    LinuxTools::collapseWindows();
#endif

	showMinimized();
	hide();

    event->ignore();
}

void MainWindow::mousePressEvent(QMouseEvent *event)
{
	if (event->button() == Qt::LeftButton)
	{
		dragPosition = event->globalPos() - frameGeometry().topLeft();
		isMousePressed = true;
		event->accept();

		return;
	}

	QMainWindow::mousePressEvent(event);
}

void MainWindow::mouseMoveEvent(QMouseEvent *event)
{
	if (event->buttons() & Qt::LeftButton && isMousePressed)
	{
		move(event->globalPos() - dragPosition);
		event->accept();

		return;
	}

	QMainWindow::mouseMoveEvent(event);
}

void MainWindow::mouseReleaseEvent(QMouseEvent *event)
{
	if (event->button() == Qt::LeftButton)
	{
		isMousePressed = false;
	}

	QMainWindow::mouseReleaseEvent(event);
}

void MainWindow::resizeEvent(QResizeEvent *event)
{
	if (!(windowState() & Qt::WindowMaximized))
		currentSize = size();

	drawingWindow->updatePosition(this);
 	historyWindow->updatePosition(this);
	contactList->updatePosition(this);

	updateMask();
	
	QMainWindow::resizeEvent(event);
}

void MainWindow::moveEvent(QMoveEvent *event)
{
	 if (!(windowState() & Qt::WindowMaximized))
		currentPosition = pos();

	 drawingWindow->updatePosition(this);
	 historyWindow->updatePosition(this);
	 contactList->updatePosition(this);

	 QMainWindow::moveEvent(event);
}

// Tray functions
void MainWindow::setupTray()
{
	trayIcon = new QSystemTrayIcon(this);
	trayIcon->setIcon(StatusIcon::getStatusIcon(UserStatus::Offline));

	trayIcon->show();
	connect(trayIcon, SIGNAL(activated(QSystemTrayIcon::ActivationReason)), this, SLOT(onTrayIconActivated(QSystemTrayIcon::ActivationReason)));

	trayMenu = new QMenu(this);

	openTvAction = trayMenu->addAction(tr(OPEN_TV_MENU_ITEM));
	connect(openTvAction, SIGNAL(triggered()), this, SLOT(showWindow()));

	trayMenu->addSeparator();

	onlineAction = trayMenu->addAction(StatusIcon::getStatusIcon(UserStatus::Online), tr(ONLINE_MENU_ITEM));	
	connect(onlineAction, SIGNAL(triggered()), this, SLOT(relogon()));

	offlineAction = trayMenu->addAction(StatusIcon::getStatusIcon(UserStatus::Offline), tr(OFFLINE_MENU_ITEM));
	connect(offlineAction, SIGNAL(triggered()), this, SLOT(offline()));

	networkSettingsAction = trayMenu->addAction(tr(NETWORK_SETTINGS_MENU_ITEM));
	connect(networkSettingsAction, SIGNAL(triggered()), this, SLOT(showSettings()));

	trayMenu->addSeparator();

	signOutAction = trayMenu->addAction(tr(SIGNOUT_MENU_ITEM));
	connect(signOutAction, SIGNAL(triggered()), this, SLOT(logout()));

	exitAction = trayMenu->addAction(tr(EXIT_MENU_ITEM));
	connect(exitAction, SIGNAL(triggered()), this, SLOT(shutDown()));
}

void MainWindow::toggleTrayMenuTimer()
{
	if (trayClickTimer < 0)
	{
		trayClickPosition = QCursor::pos();
		trayClickTimer = startTimer(QApplication::doubleClickInterval() + 1);
	}
	else
		stopTrayMenuTimer();
}

void MainWindow::stopTrayMenuTimer()
{
	if (trayClickTimer < 0)
		return;

	killTimer(trayClickTimer);
	trayClickTimer = -1;
}

void MainWindow::stopTrayConnectingTimer()
{
	if (trayConnectingTimer < 0)
		return;

	killTimer(trayConnectingTimer);
	trayConnectingTimer = -1;
}

// Window functions
void MainWindow::showWindow()
{
	bool isActive = isActiveOrHasActiveChild();

	if (!isVisible() || isMinimized())
	{
#ifdef Q_WS_MAC
        MacTools::restoreWindows();
#endif
#ifdef Q_WS_X11
        LinuxTools::restoreWindows();
#endif

        showNormal();
	}
	
	if(!isActive)
	{
		activateWindow();
	}
}

void MainWindow::showWidget(MainWindow::ScreenWidget showWidget)
{
	stackedWidget->setCurrentIndex((int) showWidget);	
}

void MainWindow::resetWindowGeometry()
{
	resize(MAINWINDOW_INITIAL_WIDTH, MAINWINDOW_INITIAL_HEIGHT);

	UiTools::moveWindowToScreenCenter(this);
}

void MainWindow::updateWindowTitle()
{
	if (INFOMANAGER->getIsLoggedOn())
	{
		QString login = INFOMANAGER->getUserAccount().getLogin();

		setWindowTitle(tr(APP_TITLE1).arg(login));
		trayIcon->setToolTip(windowTitle());
	}
	else
	{
		setWindowTitle(tr(APP_TITLE));
		trayIcon->setToolTip(tr(APP_TITLE));
	}
}

// Child windows functions
void MainWindow::createChildWindows()
{
	currentRecipient = ALL_FRIENDS_ID;

	// Creating drawing window
	drawingWindow = new DrawingWindow(this);

	connect(drawingWindow, SIGNAL(clipartWindowRequest()), this, SLOT(onShowClipart()));
	connect(drawingWindow, SIGNAL(messageSent()), this, SLOT(onMessageSent()));
	connect(drawingWindow, SIGNAL(replyChanged()), tvWidget, SLOT(replyChanged()));
	
	drawingWindow->updatePosition(this);

	historyWindow = new HistoryWindow(this);
	historyWindow->setTvWidget(tvWidget);

	connect(historyWindow, SIGNAL(beginOfPreview(const MessageKey &)), tvWidget, SLOT(showHistoryMessage(const MessageKey &)));
	connect(historyWindow, SIGNAL(endOfPreview()), tvWidget, SLOT(stopShowingHistory()));
	connect(historyWindow, SIGNAL(clicked(const MessageKey &)), tvWidget, SLOT(scrollToMessage(const MessageKey &)));
	connect(historyWindow, SIGNAL(deleteMessages(const QList<MessageKey> &)), tvWidget, SLOT(deleteMessages(const QList<MessageKey> &)));
	connect(historyWindow, SIGNAL(saveMessage()), tvWidget, SLOT(saveMessage()));
	connect(historyWindow, SIGNAL(copyMessage()), tvWidget, SLOT(copyMessage()));

	connect(tvWidget, SIGNAL(publishRequest(const MessageKey &)), historyWindow, SLOT(publishMessage(const MessageKey &)));

	historyWindow->updatePosition(this);

	contactList = new NewContactListWindow(this);
	contactList->updatePosition(this);
	contactList->initAutoUpdater(&autoUpdater);
	
	// Connect menu signals
	connect(contactList, SIGNAL(goOnline()), this, SLOT(relogon()));
	connect(contactList, SIGNAL(goOffline()), this, SLOT(offline()));
	connect(contactList, SIGNAL(goLogout()), this, SLOT(logout()));
	connect(contactList, SIGNAL(goExit()), this, SLOT(shutDown()));
	connect(contactList, SIGNAL(contactSelected(qint32)), this, SLOT(onContactSelected(qint32)));
	connect(contactList, SIGNAL(contactImageDrop(qint32, const QImage &)), this, SLOT(onContactImageDrop(qint32, const QImage &)));
	connect(contactList, SIGNAL(soundStateChanged(const bool)), this, SLOT(onSoundStateChanged(const bool)));
	connect(contactList, SIGNAL(unreadContacts(int)), contactListButton, SLOT(update(int)));
#ifdef Q_WS_MAC
    connect(contactList, SIGNAL(unreadContacts(int)), DockIconHandler::instance(), SLOT(unreadCountChanged(int)));
#endif
	connect(contactList, SIGNAL(historyCleared()), tvWidget, SLOT(reload()));

	connect(welcomeWidget, SIGNAL(showProfileDialog()), contactList, SLOT(onShowProfileDialog()));
	connect(welcomeWidget, SIGNAL(showFindFriendsDialog()), contactList, SLOT(onShowFindDialog()));
}

void MainWindow::resetChildWindows()
{
	clipartWindow->hide();
	
	historyButton->setChecked(false);
	drawButton->setChecked(false);

	drawingWindow->reset();

	contactListButton->setChecked(false);
	contactList->clearContacts();

	tvWidget->reset();
}

void MainWindow::updateDrawingWindow(qint32 id)
{
	if (!drawButton->isChecked())
		return;

	MessageType::MessageTypeEnum type = MessageType::Private;
	if (id == ALL_FRIENDS_ID)
	{
		type = MessageType::Friends;
	}
	else
	{
		ContactInfo *contact = INFOMANAGER->getContact(id);
		if (contact != NULL && contact->getType() == ContactType::Channel)
		{
			type = MessageType::Channel;
		}
	}
	drawingWindow->setRecipient(type, id);
}

void MainWindow::updateHistoryWindow(qint32 id)
{
	if (!historyButton->isChecked())
		return;

	historyWindow->setRecipient( (id == ALL_FRIENDS_ID) ? MessageType::Friends : MessageType::Private, id);
}

void MainWindow::saveSettings() const
{
	QSettings settings;
	settings.beginGroup(MAIN_WINDOW_GROUP);
	settings.setValue(SIZE_OPTION, currentSize);

	settings.setValue(POSITION_OPTION, currentPosition);
	settings.setValue(STATE_OPTION, (int) windowState());

	settings.setValue(DRAWING_WINDOW_UNDOCKED_STATE_OPTION, drawingWindow->getUnDockedState());
	settings.setValue(DRAWING_WINDOW_POSITION_OPTION, drawingWindow->pos());

	settings.endGroup();
}

void MainWindow::loadSettings()
{
	QSettings settings;
	settings.beginGroup(MAIN_WINDOW_GROUP);

	QRect availableGeometry = QApplication::desktop()->availableGeometry(this);
	
	currentSize = settings.value(SIZE_OPTION).toSize();
	
	if (!currentSize.isValid() 
		|| (currentSize.width() >= availableGeometry.width() || currentSize.height() >= availableGeometry.height()))
	{
		resetWindowGeometry();
	}
	else
	{
		availableGeometry.adjust(-(currentSize.width() / 2), -(currentSize.height() / 2), 0, 0);

		resize(currentSize);
		currentPosition = settings.value(POSITION_OPTION, QPoint(availableGeometry.width() / 2, availableGeometry.height() / 2)).toPoint();

		if (!availableGeometry.contains(currentPosition))
		{
			UiTools::moveWindowToScreenCenter(this);
		}
		else
		{
			move(currentPosition);
		}
	}

	Qt::WindowStates state = Qt::WindowStates(settings.value(STATE_OPTION).toInt());
	
	if ((state & Qt::WindowMinimized) == 0)
		setWindowState(state);

	bool isDrawingWindowUnDocked = settings.value(DRAWING_WINDOW_UNDOCKED_STATE_OPTION).toBool();
	drawingWindow->setUnDockedState(isDrawingWindowUnDocked);
	if (isDrawingWindowUnDocked)
	{
		QPoint drawingPosition = settings.value(DRAWING_WINDOW_POSITION_OPTION, QPoint(availableGeometry.width() / 2, availableGeometry.height() / 2)).toPoint();
		if (!availableGeometry.contains(drawingPosition))
		{
			UiTools::moveWindowToScreenCenter(drawingWindow);
		}
		else
		{
			drawingWindow->move(drawingPosition);
		}
	}

	settings.endGroup();
}

void MainWindow::showSettings()
{
	contactList->onShowNetworkSettingsDialog();
}

void MainWindow::ipcMessage(const QString &message)
{
	if (message == SINGLE_APP_MESSAGE)
		return;

	QStringList list = message.split(QRegExp("[\\:\\?\\s+]"), QString::SkipEmptyParts);

	int i = list.indexOf(ADD_FRIEND_COMMAND);

	if (i < 1)
		return;

	QByteArray loginBytes;
	loginBytes.append(list.at(i - 1));

	QUrl decoded = QUrl::fromEncoded(loginBytes);
	postAddFriendLogin(decoded.toString());
}

void MainWindow::relogon()
{
	offline(false);
	logonWidget->relogon();
}

void MainWindow::offline(bool userChoosenOffline)
{
	hideWaitingWidget();
	stop();

	if (userChoosenOffline)
		LOGONPROVIDER->saveStatus(UserStatus::Offline);

	INFOMANAGER->setStatus(UserStatus::Offline);
	INFOMANAGER->fadeContactList();

	isLoggingOn = false;
}

bool MainWindow::logout(bool isShuttingDown)
{
	if (isShuttingDown)
	{
		this->isShuttingDown = true;
	}

	offline(!isShuttingDown);

	if (isShuttingDown)
	{
		trayIcon->setVisible(false);
		hide();
	}

	resetChildWindows();
	buttonsFrame->hide();

	saveSettings();

	cleanup();
	
	if(!isShuttingDown)
	{
		updateWindowTitle();
		showLogonWidget();
	}

	return true;
}

void MainWindow::shutDown()
{
	bool isOnline = INFOMANAGER->isUserOnline();

	if (isShuttingDown || !logout(true))
		return;

	if (isOnline)
	{
		// wait a little for logoutCompleted
		QTimer::singleShot(2000, this, SLOT(forceQuit()));
	}
	else
	{
		QDEBUG("MainWindow::shutDown() - not online, quit immediately");
		forceQuit();
	}
    
#ifdef Q_WS_MAC
    macCleanup();
#endif
}

void MainWindow::onLogoutCompleted()
{
	if(isShuttingDown)
	{
		QDEBUG("MainWindow::onLogoutCompleted() - shutting down, forcing quit");
		forceQuit();
	}
}

void MainWindow::init()
{
	if(!INFOMANAGER->getIsLoggedOn())
		INFOMANAGER->loggedOn();

	CHANNELMANAGER->load();

	// to emulate first message from Moodbox
	if (showWelcomeScreen)
	{
		showWelcomeScreen = false;
		MessageKey messageKey = MessageKey();
		QString fileName = MessageOrganizer::getFileName(messageKey);
		QString historyName = MessageOrganizer::getFriendsMessageFileName(fileName);
		QString previewFileName = MessageOrganizer::getPreviewFileName(historyName);
		QString thumbnailFileName = MessageOrganizer::getThumbnailFileName(previewFileName);

		// first file
		QFile file(historyName);
		file.open(QIODevice::ReadWrite);
		QString message = QString(FIRST_MESSAGE_INTERNAL_FORMAT).arg(messageKey.getDate().toString(MESSAGE_TIMESTAMP));
		file.write(QByteArray().append(message));
		file.flush();
		file.close();
		// second file
#ifdef RUSSIAN_VERSION 
		QImage image(FIRST_MESSAGE_RU);
#else 
		QImage image(FIRST_MESSAGE);
#endif
		image.save(previewFileName);
		// third file
#ifdef RUSSIAN_VERSION 
		image.load(FIRST_MESSAGE_THUMBNAIL_RU);
#else 
		image.load(FIRST_MESSAGE_THUMBNAIL);
#endif
		image.save(thumbnailFileName);
	}

	MESSAGEMANAGER->initMessaging();
	
	drawingWindow->setRecipient(MessageType::Friends);
	tvWidget->setRecipient(MessageType::Friends);
	historyWindow->setRecipient(MessageType::Friends);
}

void MainWindow::cleanup()
{
	if(INFOMANAGER->getIsLoggedOn())
		INFOMANAGER->loggedOut();

	MESSAGEMANAGER->cleanupMessaging();	

	historyWindow->cleanup();
}

void MainWindow::start()
{	
	INFOMANAGER->reloadData();
	MESSAGEMANAGER->startMessageProcessing();

	INFOMANAGER->changeIsOnline(true);

	if (!friendLoginFromUrl.isEmpty())
		QTimer::singleShot(2000, this, SLOT(addFriendByLogin()));
}

void MainWindow::stop()
{
	if(INFOMANAGER->isUserOnline())
		INFOMANAGER->changeIsOnline(false);

	logonWidget->cancel();

	MESSAGEMANAGER->stopMessageProcessing();
	SERVER->logout();
}

void MainWindow::updateMask()
{
	setMask(WidgetMaskCreator::createMask(WidgetMaskCreator::WidgetMaskMain, width(), height()));
}

void MainWindow::alertWindow()
{
	bool isActive = isActiveOrHasActiveChild();

	if (!isVisible())
	{
		showMinimized();
	}

	if (isMinimized() || !isActive)
	{
		AppTools::alertWidget(this);
	}
}

void MainWindow::alertWindowIfVisible()
{
	if (isVisible())
	{
		if (isMinimized() || !isActiveOrHasActiveChild())
		{
			AppTools::alertWidget(this);
		}
	}
}

bool MainWindow::isActiveOrHasActiveChild() const
{
	QWidget *active = QApplication::activeWindow();

	while (active != NULL)
	{
		if(active == this)
			return true;

		active = active->parentWidget();
	}

	return false;
}

void MainWindow::postAddFriendLogin(const QString &login)
{
	QValidator *loginValidator = LineEditVerifier::createLoginValidator();
	int i = 0;
	QString loginCheck(login);
	QValidator::State state = loginValidator->validate(loginCheck, i);

	delete loginValidator;
	
	if (state != QValidator::Acceptable)
	{
		QMessageBox::warning(this, tr(CHECK_ERROR_TITLE), tr(CHECK_LOGIN_ERROR_MESSAGE)); 
		return;
	}
	else
		friendLoginFromUrl = login;

	if (INFOMANAGER->isUserOnline())
		addFriendByLogin();
}

void MainWindow::addFriendByLogin()
{
	if (friendLoginFromUrl == LOGONPROVIDER->getCurrentLogin())
	{
		QMessageBox::warning(this, tr(CHECK_ERROR_TITLE), tr(AUTH_REQUEST_SELF_ERROR)); 		
	}
	else
	{
		AuthorizationDialog *authDialog = new AuthorizationDialog(this);
		authDialog->request(friendLoginFromUrl);
		authDialog->setAttribute(Qt::WA_DeleteOnClose);
	}

	friendLoginFromUrl.clear();
}

void MainWindow::forceQuit()
{
	QCoreApplication::quit();
}

void MainWindow::onTrayIconActivated(QSystemTrayIcon::ActivationReason reason)
{
	if (reason == QSystemTrayIcon::DoubleClick)
		showWindow();
	else 
		if (reason == QSystemTrayIcon::Trigger) 
			toggleTrayMenuTimer();
		else 
			if (reason == QSystemTrayIcon::Context)
				showTrayMenu(QCursor::pos());
}

void MainWindow::on_contactListButton_toggled(bool checked)
{
	if (checked)
	{
		contactList->show();

		if (clipartWindow->isVisible())
			clipartWindow->raise();

		contactList->activateWindow();
	}
	else
		contactList->hide();

	updateMask();
}

void MainWindow::on_drawButton_toggled(bool checked)
{
	if (checked)
	{
		updateDrawingWindow(currentRecipient);
		drawingWindow->show();

		if (showClipartWindow)
			clipartWindow->show();

		if (clipartWindow->isVisible())
			clipartWindow->raise();
	}
	else
	{
		drawingWindow->hide();

		showClipartWindow = clipartWindow->isVisible();
		clipartWindow->hide();
	}
}

void MainWindow::on_historyButton_toggled(bool checked)
{
	if (checked)
	{
		updateHistoryWindow(currentRecipient);
		historyWindow->show();

		if (clipartWindow->isVisible())
			clipartWindow->raise();

		historyWindow->activateWindow();
	}
	else
		historyWindow->hide();

	updateMask();
}

void MainWindow::onInviteCodeAccepted()
{
	registrationWidget->setInviteCode(inviteCodeWidget->getInviteCode());
	showRegistrationWidget();
}

void MainWindow::onDownloadNewVersion()
{
	startInstallNewVersion();
}

void MainWindow::onRegistrationSuccess()
{
	stopWaitingWidget();
	showWelcomeScreen = true;
	
	logonWidget->setLoginAndPassword(registrationWidget->getLogin(), registrationWidget->getPassword());
	infoWidget->setTitleLabel(tr(REGISTERED_INFO_WIDGET_TITLE));
	infoWidget->setDescriptionLabel(tr(REGISTERED_INFO_WIDGET_TEXT).arg(registrationWidget->getLogin()));
	showInfoWidget();

	registrationWidget->clearData();
	inviteCodeWidget->clearData();
}

void MainWindow::onResetPassword()
{
	infoWidget->setTitleLabel(tr(PASSWORD_INFO_WIDGET_TITLE));
	infoWidget->setDescriptionLabel(tr(PASSWORD_INFO_WIDGET_TEXT));
	showInfoWidget();

	forgotPasswordWidget->clearData();
}

void MainWindow::onShowClipart()
{
	if (clipartWindow->isShowFirstTime)
	{
		QRect availableGeometry = QApplication::desktop()->availableGeometry(drawingWindow);
		QPoint p = drawingWindow->mapToGlobal(QPoint(drawingWindow->width() + clipartWindow->width(), 0));

		if (p.x() > availableGeometry.right())
			clipartWindow->move(drawingWindow->mapToGlobal(QPoint(-clipartWindow->width(), 0)));
		else
			clipartWindow->move(drawingWindow->mapToGlobal(QPoint(drawingWindow->width(), 0)));
	}

	clipartWindow->setVisible(!clipartWindow->isVisible());
	if (clipartWindow->isVisible())
	{
		clipartWindow->show();
		clipartWindow->raise();
	}
	clipartWindow->isShowFirstTime = false;
}

void MainWindow::onReply(const QImage &image)
{
	drawButton->setChecked(true);

	drawingWindow->addReply(image);
}

void MainWindow::showTvWidget()
{
	showWidget(TV);
}

void MainWindow::showLogonWidget(bool updateFields)
{
	logonWidget->prepareToShow(updateFields);
	showWidget(Logon);
}

void MainWindow::showInviteCodeWidget()
{
	showWidget(Invite);
}

void MainWindow::showRegistrationWidget() 
{
	showWidget(Registration);
}

void MainWindow::showForgotPasswordWidget() 
{
	forgotPasswordWidget->setLoginField(logonWidget->loginCombo->lineEdit()->text());
	showWidget(ForgotPassword);
}

void MainWindow::showInfoWidget()
{
	showWidget(Info);
}

void MainWindow::showWaitingWidget()
{
	widgetIndexBeforeWaiting = stackedWidget->currentIndex();
	showWidget(Waiting);
	waitingWidget->startWaitingTimer();
}

void MainWindow::stopWaitingWidget()
{
	waitingWidget->stopWaitingTimer();
}

void MainWindow::hideWaitingWidget()
{
	if (widgetIndexBeforeWaiting >= 0)
	{
		waitingWidget->stopWaitingTimer();
		showWidget((MainWindow::ScreenWidget) widgetIndexBeforeWaiting);
		widgetIndexBeforeWaiting = -1;
	}
}

void MainWindow::cancelWaitingWidget()
{
	if (widgetIndexBeforeWaiting >= 0)
	{
		waitingWidget->stopWaitingTimer();
		showWidget((MainWindow::ScreenWidget) widgetIndexBeforeWaiting);
		dynamic_cast<ServerWidget*>(stackedWidget->currentWidget())->onRequestCancelled();
		widgetIndexBeforeWaiting = -1;
	}
}

void MainWindow::onRegistrationBack()
{
	if (isInviteCodeNeed)
		showInviteCodeWidget();
	else
		showLogonWidget();
}

void MainWindow::onInviteCodeNeeded()
{
	isInviteCodeNeed = true;

	showInviteCodeWidget();
}

void MainWindow::onContactSelected(qint32 id)
{
	currentRecipient = id;

	if (id == ALL_FRIENDS_ID)
	{
		tvWidget->setChatLabel(tr(ALL_FRIENDS_CHAT_LABEL));
		tvWidget->setRecipient(MessageType::Friends);
	}
	else
		if (id == INFOMANAGER->getUserAccount().getId())
		{
			tvWidget->setChatLabel(tr(SANDBOX_CHAT_LABEL));
			tvWidget->setRecipient(MessageType::Private, id);
		}
		else
		{
			tvWidget->setChatLabel(INFOMANAGER->getContact(id)->getDisplayName());
			tvWidget->setRecipient(MessageType::Private, id, INFOMANAGER->getContact(id)->getType());
		}

	updateHistoryWindow(id);
	updateDrawingWindow(id);
}

void MainWindow::onContactImageDrop(qint32 id, const QImage &image)
{
	onContactSelected(id);
	drawButton->setChecked(true);
	drawingWindow->addImage(image);
}

void MainWindow::onSoundStateChanged(bool enableSounds)
{
	isPlaySounds = enableSounds;
}

void MainWindow::onFriendsMessageReceived(const MessageKey &)
{
	bool noMessages = contactList->getUnreadMessagesForAllFriends() == 0;

	if (isPlaySounds && noMessages)
		QSound::play(AppTools::addPathSeparator(AppTools::getSoundsResourcesFolder()) + FRIEND_MESSAGE_RECEIVED_SOUND);

	alertWindowIfVisible();

	contactList->onMessageReceived(0, true);
}

void MainWindow::onMessageSent()
{
	if (isPlaySounds)
		QSound::play(AppTools::addPathSeparator(AppTools::getSoundsResourcesFolder()) + MESSAGE_SENT_SOUND);
}

void MainWindow::onPrivateMessageReceived(qint32 authorId, const MessageKey &key)
{
	Q_UNUSED(key);
	
	bool noMessages = contactList->getUnreadMessagesForContact(authorId) == 0;

	if (isPlaySounds && noMessages)
		QSound::play(AppTools::addPathSeparator(AppTools::getSoundsResourcesFolder()) + FRIEND_MESSAGE_RECEIVED_SOUND);

	alertWindow();
	
	contactList->onMessageReceived(authorId);
}

void MainWindow::onChannelMessageReceived(qint32 channelId, const MessageKey &key)
{
	Q_UNUSED(key);

	bool shouldAlert = (contactList->getUnreadMessagesForContact(channelId) == 0) && CHANNELMANAGER->getChannelNotifications(channelId);	

	if (shouldAlert)
	{
		bool shouldPlay = isPlaySounds && currentRecipient != channelId;

		if (shouldPlay)
			QSound::play(AppTools::addPathSeparator(AppTools::getSoundsResourcesFolder()) + MESSAGE_SENT_SOUND);

		alertWindowIfVisible();
	}

	contactList->onMessageReceived(channelId);
}

void MainWindow::showTrayMenu(const QPoint &pos)
{
	if (INFOMANAGER->getIsLoggedOn())
	{
		UserStatus::UserStatusEnum status = INFOMANAGER->getUserStatus();

		networkSettingsAction->setVisible(false);
		onlineAction->setVisible(true);
		onlineAction->setEnabled(status != UserStatus::Online);
		offlineAction->setVisible(true);
		offlineAction->setEnabled(status != UserStatus::Offline);
		signOutAction->setVisible(true);
	}
	else
	{
		networkSettingsAction->setVisible(true);
		onlineAction->setVisible(false);
		offlineAction->setVisible(false);
		signOutAction->setVisible(false);
	}
	
	trayMenu->popup(pos);
	trayMenu->activateWindow();

	activateWindow();
}

void MainWindow::showHelp()
{
	UiTools::showHelp();
}

void MainWindow::startInstallNewVersion()
{
	if(http == NULL)
	{
		http = new QNetworkAccessManager(this);
		connect(http, SIGNAL(finished(QNetworkReply *)), this, SLOT(requestFinished(QNetworkReply *)));
	}

	if(progressDialog == NULL)
	{
		progressDialog = new QProgressDialog(tr(DOWLOADING_PROGRESS_MESSAGE), tr(PROGRESS_CANCEL_TEXT), 0, 0, this);
		progressDialog->setWindowTitle(tr(APP_TITLE));
		progressDialog->setWindowModality(Qt::ApplicationModal);
		progressDialog->setWindowFlags((progressDialog->windowFlags() | Qt::Tool) & ~Qt::WindowContextHelpButtonHint & ~Qt::WindowSystemMenuHint);
		connect(progressDialog, SIGNAL(canceled()), this, SLOT(cancelHttpRequest()));

		QProgressBar *bar = new QProgressBar(progressDialog);
	    bar->setTextVisible(false);
	    progressDialog->setBar(bar);
	}

	QNetworkRequest request(QUrl(NEWEST_LIGHT_INSTALLER_URL));

	reply = http->get(request);

	progressDialog->show();

	connect(reply, SIGNAL(downloadProgress(qint64, qint64)), this, SLOT(dataProcessedProgress(qint64, qint64)));
}

#ifdef Q_WS_MAC
    
// vhbit: don't think tray icon should be removed, 
// so menu is very similar to tray, currently the only difference
// is the lack of "Exit", because it is automatically created
void MainWindow::createDockMenu() 
{
    QMenu *dockMenu = new QMenu(this);
    
    dockMenu->addAction(openTvAction);    
    dockMenu->addSeparator();
    
    dockMenu->addAction(onlineAction);	
    dockMenu->addAction(offlineAction);
    dockMenu->addAction(networkSettingsAction);
    dockMenu->addSeparator();
    dockMenu->addAction(signOutAction);
    
    qt_mac_set_dock_menu(dockMenu);
}

// vhbit: adds Mac-specific menu bar
void MainWindow::createMainMenu() 
{
    appMenuBar = new QMenuBar();
    // vhbit todo: don't use hard-coded string
    // NOTE: the name of the created item should be identical to
    // the application name
    QMenu *tmpMenu = appMenuBar->addMenu("MoodBox");
    
    // vhbit: hard-coded strings used to assign our actions for
    // the automatically created menu items
    QAction *settingsAction = tmpMenu->addAction("config");
    connect(settingsAction, SIGNAL(triggered()), contactList, SLOT(onShowNetworkSettingsDialog()));
    
    QAction *exitAction = tmpMenu->addAction("quit");
    connect(exitAction, SIGNAL(triggered()), this, SLOT(shutDown()));    
}
    
void MainWindow::macInit()
{
    createMainMenu();
    createDockMenu();
    
    AppEventHandler *handler = AppEventHandler::instance();
    handler->registerHandlerFor(ReopenApplication);
    handler->registerHandlerFor(QuitApplication);
    
    connect(handler, SIGNAL(reopenApplication()), this, SLOT(showWindow()));
    connect(handler, SIGNAL(quitApplication()), this, SLOT(shutDown()));    
}
    
void MainWindow::macCleanup()
{
    AppEventHandler *handler = AppEventHandler::instance();
    handler->unregisterHandlerFor(ReopenApplication);
    handler->unregisterHandlerFor(QuitApplication);
    AppEventHandler::cleanUp();
    MacTools::cleanUp();
    DockIconHandler::cleanUp();
    
    delete appMenuBar;
}
    
    
#endif    
    
void MainWindow::requestFinished(QNetworkReply *reply)
{
	bool isError = true;

	if (reply->error() == QNetworkReply::NoError)
	{
		QByteArray data = reply->readAll();
		QFile file(AppTools::addPathSeparator(QDir::tempPath()) + NEWEST_LIGHT_INSTALLER);
		file.open(QIODevice::WriteOnly);
		file.write(data);
		file.close();
		if (QProcess::startDetached(AppTools::addPathSeparator(QDir::tempPath()) + NEWEST_LIGHT_INSTALLER))
		{
			shutDown();
			isError = false;
		}
		else
			QMessageBox::information(this, tr(DOWNLOAD_INSTALLER_ERROR_TITLE), tr(DOWNLOAD_INSTALLER_ERROR_TEXT));
	}
	else if (reply->error() != QNetworkReply::OperationCanceledError)
	{
		QMessageBox::information(this, tr(DOWNLOAD_INSTALLER_ERROR_TITLE), tr(DOWNLOAD_INSTALLER_ERROR_TEXT));
	}
	
	if (isError)
	{
		reply->deleteLater();
		progressDialog->hide();
		reply = NULL;
	}
}

void MainWindow::dataProcessedProgress(qint64 done, qint64 total)
{
	if (progressDialog != NULL)
	{
		if (progressDialog->maximum() == 100) // by default maximum = 100
			progressDialog->setMaximum(total);
		progressDialog->setValue(done);
	}
}

void MainWindow::cancelHttpRequest()
{
	if (reply != NULL)
	{
		reply->abort();	
	}
}

}
