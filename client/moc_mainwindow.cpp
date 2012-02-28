/****************************************************************************
** Meta object code from reading C++ file 'mainwindow.h'
**
** Created: Wed Jun 24 21:01:14 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "mainwindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'mainwindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__MainWindow[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      53,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      21,   20,   20,   20, 0x0a,
      51,   44,   20,   20, 0x0a,
      99,   20,   20,   20, 0x0a,
     163,  149,   20,   20, 0x0a,
     248,   20,   20,   20, 0x0a,
     265,   20,   20,   20, 0x0a,
     282,   20,   20,   20, 0x0a,
     299,   20,   20,   20, 0x0a,
     312,   20,   20,   20, 0x0a,
     323,   20,   20,   20, 0x0a,
     343,   20,   20,   20, 0x0a,
     372,  353,   20,   20, 0x0a,
     386,   20,   20,   20, 0x2a,
     416,  401,  396,   20, 0x0a,
     429,   20,  396,   20, 0x2a,
     438,   20,   20,   20, 0x0a,
     461,  453,   20,   20, 0x0a,
     481,   20,   20,   20, 0x08,
     500,   20,   20,   20, 0x08,
     519,  512,   20,   20, 0x08,
     582,  574,   20,   20, 0x08,
     617,  574,   20,   20, 0x08,
     645,  574,   20,   20, 0x08,
     676,   20,   20,   20, 0x08,
     699,   20,   20,   20, 0x08,
     722,   20,   20,   20, 0x08,
     746,   20,   20,   20, 0x08,
     764,   20,   20,   20, 0x08,
     786,  780,   20,   20, 0x08,
     802,   20,   20,   20, 0x08,
     830,  817,   20,   20, 0x08,
     852,   20,   20,   20, 0x28,
     870,   20,   20,   20, 0x08,
     893,   20,   20,   20, 0x08,
     918,   20,   20,   20, 0x08,
     945,   20,   20,   20, 0x08,
     962,   20,   20,   20, 0x08,
     982,   20,   20,   20, 0x08,
    1002,   20,   20,   20, 0x08,
    1022,   20,   20,   20, 0x08,
    1044,   20,   20,   20, 0x08,
    1065,   20,   20,   20, 0x08,
    1089, 1086,   20,   20, 0x08,
    1124, 1115,   20,   20, 0x08,
    1171, 1158,   20,   20, 0x08,
    1210, 1197,   20,   20, 0x08,
    1268, 1254,   20,   20, 0x08,
    1312,   20,   20,   20, 0x08,
    1349,   20,   20,   20, 0x08,
    1365,   20,   20,   20, 0x08,
    1382, 1376,   20,   20, 0x08,
    1425, 1414,   20,   20, 0x08,
    1462,   20,   20,   20, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__MainWindow[] = {
    "MoodBox::MainWindow\0\0onUserAccountUpdated()\0"
    "status\0onUserStatusChanged(UserStatus::UserStatusEnum)\0"
    "onServerError(ServerResponseHandler::ServerError)\0"
    "ResultCode,id\0"
    "onShowExceptionDialogSendingMessage(ContactResultCode::ContactResultCo"
    "deEnum,qint32)\0"
    "onLogonStarted()\0onLogonStopped()\0"
    "onLogonSuccess()\0showWindow()\0shutDown()\0"
    "onLogoutCompleted()\0relogon()\0"
    "userChoosenOffline\0offline(bool)\0"
    "offline()\0bool\0isShuttingDown\0"
    "logout(bool)\0logout()\0showSettings()\0"
    "message\0ipcMessage(QString)\0"
    "addFriendByLogin()\0forceQuit()\0reason\0"
    "onTrayIconActivated(QSystemTrayIcon::ActivationReason)\0"
    "checked\0on_contactListButton_toggled(bool)\0"
    "on_drawButton_toggled(bool)\0"
    "on_historyButton_toggled(bool)\0"
    "onDownloadNewVersion()\0onInviteCodeAccepted()\0"
    "onRegistrationSuccess()\0onResetPassword()\0"
    "onShowClipart()\0image\0onReply(QImage)\0"
    "showTvWidget()\0updateFields\0"
    "showLogonWidget(bool)\0showLogonWidget()\0"
    "showInviteCodeWidget()\0showRegistrationWidget()\0"
    "showForgotPasswordWidget()\0showInfoWidget()\0"
    "showWaitingWidget()\0stopWaitingWidget()\0"
    "hideWaitingWidget()\0cancelWaitingWidget()\0"
    "onRegistrationBack()\0onInviteCodeNeeded()\0"
    "id\0onContactSelected(qint32)\0id,image\0"
    "onContactImageDrop(qint32,QImage)\0"
    "enableSounds\0onSoundStateChanged(bool)\0"
    "authorId,key\0onPrivateMessageReceived(qint32,MessageKey)\0"
    "channelId,key\0onChannelMessageReceived(qint32,MessageKey)\0"
    "onFriendsMessageReceived(MessageKey)\0"
    "onMessageSent()\0showHelp()\0reply\0"
    "requestFinished(QNetworkReply*)\0"
    "done,total\0dataProcessedProgress(qint64,qint64)\0"
    "cancelHttpRequest()\0"
};

const QMetaObject MoodBox::MainWindow::staticMetaObject = {
    { &QMainWindow::staticMetaObject, qt_meta_stringdata_MoodBox__MainWindow,
      qt_meta_data_MoodBox__MainWindow, 0 }
};

const QMetaObject *MoodBox::MainWindow::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::MainWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__MainWindow))
        return static_cast<void*>(const_cast< MainWindow*>(this));
    if (!strcmp(_clname, "MainWindowClass"))
        return static_cast< MainWindowClass*>(const_cast< MainWindow*>(this));
    return QMainWindow::qt_metacast(_clname);
}

int MoodBox::MainWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QMainWindow::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onUserAccountUpdated(); break;
        case 1: onUserStatusChanged((*reinterpret_cast< UserStatus::UserStatusEnum(*)>(_a[1]))); break;
        case 2: onServerError((*reinterpret_cast< ServerResponseHandler::ServerError(*)>(_a[1]))); break;
        case 3: onShowExceptionDialogSendingMessage((*reinterpret_cast< ContactResultCode::ContactResultCodeEnum(*)>(_a[1])),(*reinterpret_cast< qint32(*)>(_a[2]))); break;
        case 4: onLogonStarted(); break;
        case 5: onLogonStopped(); break;
        case 6: onLogonSuccess(); break;
        case 7: showWindow(); break;
        case 8: shutDown(); break;
        case 9: onLogoutCompleted(); break;
        case 10: relogon(); break;
        case 11: offline((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 12: offline(); break;
        case 13: { bool _r = logout((*reinterpret_cast< bool(*)>(_a[1])));
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 14: { bool _r = logout();
            if (_a[0]) *reinterpret_cast< bool*>(_a[0]) = _r; }  break;
        case 15: showSettings(); break;
        case 16: ipcMessage((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 17: addFriendByLogin(); break;
        case 18: forceQuit(); break;
        case 19: onTrayIconActivated((*reinterpret_cast< QSystemTrayIcon::ActivationReason(*)>(_a[1]))); break;
        case 20: on_contactListButton_toggled((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 21: on_drawButton_toggled((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 22: on_historyButton_toggled((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 23: onDownloadNewVersion(); break;
        case 24: onInviteCodeAccepted(); break;
        case 25: onRegistrationSuccess(); break;
        case 26: onResetPassword(); break;
        case 27: onShowClipart(); break;
        case 28: onReply((*reinterpret_cast< const QImage(*)>(_a[1]))); break;
        case 29: showTvWidget(); break;
        case 30: showLogonWidget((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 31: showLogonWidget(); break;
        case 32: showInviteCodeWidget(); break;
        case 33: showRegistrationWidget(); break;
        case 34: showForgotPasswordWidget(); break;
        case 35: showInfoWidget(); break;
        case 36: showWaitingWidget(); break;
        case 37: stopWaitingWidget(); break;
        case 38: hideWaitingWidget(); break;
        case 39: cancelWaitingWidget(); break;
        case 40: onRegistrationBack(); break;
        case 41: onInviteCodeNeeded(); break;
        case 42: onContactSelected((*reinterpret_cast< qint32(*)>(_a[1]))); break;
        case 43: onContactImageDrop((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const QImage(*)>(_a[2]))); break;
        case 44: onSoundStateChanged((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 45: onPrivateMessageReceived((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const MessageKey(*)>(_a[2]))); break;
        case 46: onChannelMessageReceived((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const MessageKey(*)>(_a[2]))); break;
        case 47: onFriendsMessageReceived((*reinterpret_cast< const MessageKey(*)>(_a[1]))); break;
        case 48: onMessageSent(); break;
        case 49: showHelp(); break;
        case 50: requestFinished((*reinterpret_cast< QNetworkReply*(*)>(_a[1]))); break;
        case 51: dataProcessedProgress((*reinterpret_cast< qint64(*)>(_a[1])),(*reinterpret_cast< qint64(*)>(_a[2]))); break;
        case 52: cancelHttpRequest(); break;
        default: ;
        }
        _id -= 53;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
