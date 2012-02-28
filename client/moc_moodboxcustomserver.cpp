/****************************************************************************
** Meta object code from reading C++ file 'moodboxcustomserver.h'
**
** Created: Wed Jun 24 21:01:15 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "moodboxcustomserver.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'moodboxcustomserver.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__MoodBoxCustomServer[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      35,   30,   29,   29, 0x05,
      90,   83,   29,   29, 0x05,
     130,   29,   29,   29, 0x05,

 // slots: signature, parameters, type, tag, flags
     161,  148,   29,   29, 0x09,
     209,   29,   29,   29, 0x09,
     241,  224,   29,   29, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__MoodBoxCustomServer[] = {
    "MoodBox::MoodBoxCustomServer\0\0code\0"
    "serverError(ServerResponseHandler::ServerError)\0"
    "result\0notificationResult(QList<Notification>)\0"
    "logoutCompleted()\0fault,result\0"
    "onNotificationResult(Fault,QList<Notification>)\0"
    "onDisconnect()\0state,fault,info\0"
    "onGetServerInfo(QVariant,Fault,ServerInfo)\0"
};

const QMetaObject MoodBox::MoodBoxCustomServer::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MoodBox__MoodBoxCustomServer,
      qt_meta_data_MoodBox__MoodBoxCustomServer, 0 }
};

const QMetaObject *MoodBox::MoodBoxCustomServer::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::MoodBoxCustomServer::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__MoodBoxCustomServer))
        return static_cast<void*>(const_cast< MoodBoxCustomServer*>(this));
    if (!strcmp(_clname, "MoodBoxServer"))
        return static_cast< MoodBoxServer*>(const_cast< MoodBoxCustomServer*>(this));
    return QObject::qt_metacast(_clname);
}

int MoodBox::MoodBoxCustomServer::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: serverError((*reinterpret_cast< ServerResponseHandler::ServerError(*)>(_a[1]))); break;
        case 1: notificationResult((*reinterpret_cast< QList<Notification>(*)>(_a[1]))); break;
        case 2: logoutCompleted(); break;
        case 3: onNotificationResult((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< QList<Notification>(*)>(_a[2]))); break;
        case 4: onDisconnect(); break;
        case 5: onGetServerInfo((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< ServerInfo(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 6;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::MoodBoxCustomServer::serverError(ServerResponseHandler::ServerError _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::MoodBoxCustomServer::notificationResult(QList<Notification> _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void MoodBox::MoodBoxCustomServer::logoutCompleted()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}
static const uint qt_meta_data_MoodBox__AuthTicketResultCallbackDelegate[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      62,   43,   42,   42, 0x05,

 // slots: signature, parameters, type, tag, flags
     107,   43,   42,   42, 0x09,
     162,   43,   42,   42, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__AuthTicketResultCallbackDelegate[] = {
    "MoodBox::AuthTicketResultCallbackDelegate\0"
    "\0state,fault,result\0"
    "logonResult(QVariant,Fault,AuthTicketResult)\0"
    "onGetAuthTicketResult(QVariant,Fault,AuthTicketResult)\0"
    "onNotificationRegister(QVariant,Fault,NotificationRegistrationResult)\0"
};

const QMetaObject MoodBox::AuthTicketResultCallbackDelegate::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MoodBox__AuthTicketResultCallbackDelegate,
      qt_meta_data_MoodBox__AuthTicketResultCallbackDelegate, 0 }
};

const QMetaObject *MoodBox::AuthTicketResultCallbackDelegate::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::AuthTicketResultCallbackDelegate::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__AuthTicketResultCallbackDelegate))
        return static_cast<void*>(const_cast< AuthTicketResultCallbackDelegate*>(this));
    return QObject::qt_metacast(_clname);
}

int MoodBox::AuthTicketResultCallbackDelegate::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: logonResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< AuthTicketResult(*)>(_a[3]))); break;
        case 1: onGetAuthTicketResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< AuthTicketResult(*)>(_a[3]))); break;
        case 2: onNotificationRegister((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< NotificationRegistrationResult(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::AuthTicketResultCallbackDelegate::logonResult(QVariant _t1, Fault _t2, AuthTicketResult _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
