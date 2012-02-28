#ifndef UITOOLS_H
#define UITOOLS_H

#include <QWidget>
#include <QString>
#include <QDialog>
#include <QBitmap>
#include <QMessageBox>
#include <QLocale>

#include <QTableWidget>
#include <QMouseEvent>

#include "contactinfo.h"
#include "fault.h"
#include "debug.h"
#include "language.h"

class QMouseEvent;
class QMenu;

namespace MoodBox
{

//#define SERVER_ERROR_TITLE				QT_TRANSLATE_NOOP("MoodBox::UiTools", "ServerErrorTitle")
//#define REQUEST_ERROR_TITLE				QT_TRANSLATE_NOOP("MoodBox::UiTools", "RequestErrorTitle")

#define FAULT_TEXT_TRANSPORT_ERROR		QT_TRANSLATE_NOOP("MoodBox::UiTools", "NetworkError%1")
#define FAULT_TEXT_REQUEST_TOO_LARGE	QT_TRANSLATE_NOOP("MoodBox::UiTools", "RequestTooLarge")
#define FAULT_TEXT_REQUEST_CANCELLED	QT_TRANSLATE_NOOP("MoodBox::UiTools", "RequestIsCancelled")
#define FAULT_TEXT_REQUEST_TIMED_OUT	QT_TRANSLATE_NOOP("MoodBox::UiTools", "RequestIsTimedOut")
#define FAULT_TEXT_PARSER_ERROR			QT_TRANSLATE_NOOP("MoodBox::UiTools", "CannotParseServerResponse")

#define ADD_USER_MENU_ITEM				QT_TRANSLATE_NOOP("@default", "AddUserMenuItem")
#define USER_INFO_MENU_ITEM				QT_TRANSLATE_NOOP("@default", "UserInfoMenuItem")
#define WEB_PROFILE_MENU_ITEM			QT_TRANSLATE_NOOP("@default", "WebProfileMenuItem")

// Status Icons
#define NOTAUTH_STATUS_ICON		":/MoodBox/Resources/not_authorized_icon.png"
#define OFFLINE_STATUS_ICON		":/MoodBox/Resources/offline_icon.png"
#define ONLINE_STATUS_ICON		":/MoodBox/Resources/online_icon.png"

#define CHANNEL_DEFAULT_AVATAR		":/MoodBox/Resources/avatar_channel_default.png"

#ifndef RUSSIAN_VERSION
  #define HELP_URL						"http://moodbox.com/help"
  // User profile on the web link
  #define PROFILE_LINK					"http://moodbox.com/user/%1"
  // Channel profile on the web link
  #define CHANNEL_LINK					"http://moodbox.com/channel/%1"
#else
  #define HELP_URL						"http://risovaska.ru/help"
  #define PROFILE_LINK					"http://risovaska.ru/user/%1"
  #define CHANNEL_LINK					"http://risovaska.ru/channel/%1"
#endif

class UiTools
{
public:
	static QString getFaultTitle(Fault fault);
	static QString getFaultText(Fault fault);

	static void handleError(QWidget *parent, QString title, Fault fault);
	static void handleError(QWidget *parent, QString title, QString text);

	static QMessageBox::StandardButton showDialog(QWidget *parent, QString title, QString text, QMessageBox::StandardButtons buttons);
	
	static void moveWindowToScreenCenter(QWidget *dialog);
	static QString createHtmlLink(const QString &style, const QString &value);

	static QString getCountryName(QLocale::Country);

	static QString cropTextByRect(QFontMetrics *fontMetrics, QRect &rect, QString text, int textOption, bool &isCropped);

	// Function for sorting contacts
	static bool contactLessThan(ContactInfo contact1, ContactInfo contact2);

	static void showHelp();

	static bool rmDirRecursive(QString path);
private:
};

// Typical dialog without frame and movable by mouse click on client area
class MoodBoxDialog : public QDialog
{
	Q_OBJECT

public:
    // vhbit: if all dialogs should be without a title bar
    // we can add Qt::CustomizeWindowHint here for any OS
	MoodBoxDialog(QWidget *parent = NULL, Qt::WindowFlags flags = Qt::FramelessWindowHint | Qt::Dialog | Qt::CustomizeWindowHint);
    virtual ~MoodBoxDialog();

protected:
	QPoint dragPosition;
	bool isMousePressed;

	virtual void mousePressEvent(QMouseEvent *event);
	virtual void mouseMoveEvent(QMouseEvent *event);
	virtual void mouseReleaseEvent(QMouseEvent *event);
	virtual void resizeEvent(QResizeEvent *event);
    
	// Localization
	void changeEvent(QEvent *event);
	virtual void retranslate() {};
};

    
    
// Widget movable by mouse click on client area
class MovableWidget : public QWidget
{
	Q_OBJECT

public:
	MovableWidget(QWidget *parent = NULL, Qt::WFlags flags = Qt::Widget);
    virtual ~MovableWidget();

signals:
	void dragged(const QPoint &newPos);

protected:
	QPoint dragPosition;
	bool isMousePressed;

	virtual void mousePressEvent(QMouseEvent *event);
	virtual void mouseMoveEvent(QMouseEvent *event);
	virtual void mouseReleaseEvent(QMouseEvent *event);
};

// helper class for creating mask for widgets (round edges & others effects)
class WidgetMaskCreator
{
public:
	enum WidgetMaskType {WidgetMaskMain, WidgetMaskSmall, ContactListMenu, WidgetMaskHistory, WidgetMaskContacts};
	static QBitmap createMask(WidgetMaskType maskType, int width, int height);
};

// ContactTable (with context menu)
class ContactTable : public QTableWidget
{
	Q_OBJECT;

public:
	ContactTable(QWidget *parent = NULL);

signals:
	void addUserTriggered();
	void userInfoTriggered(int row);
	void webProfileTriggered(int row);

protected:
	virtual void mousePressEvent(QMouseEvent *event);

private:
	QMenu *contextMenu;
	int currentRow;

	void createContextMenu();

private slots:
	void onUserInfoTriggered();
	void onWebProfileTriggered();
};

}

#endif // UITOOLS_H
