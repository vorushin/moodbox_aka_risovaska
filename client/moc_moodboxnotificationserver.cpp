/****************************************************************************
** Meta object code from reading C++ file 'moodboxnotificationserver.h'
**
** Created: Wed Jun 24 21:01:15 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "moodboxnotificationserver.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'moodboxnotificationserver.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__MoodBoxNotificationServer[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       8,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      56,   36,   35,   35, 0x05,
     102,   35,   35,   35, 0x05,
     120,   35,   35,   35, 0x05,

 // slots: signature, parameters, type, tag, flags
     154,   35,   35,   35, 0x0a,
     201,  182,   35,   35, 0x09,
     255,  182,   35,   35, 0x09,
     308,   35,   35,   35, 0x09,
     335,  182,   35,   35, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__MoodBoxNotificationServer[] = {
    "MoodBox::MoodBoxNotificationServer\0\0"
    "fault,notifications\0"
    "notificationResult(Fault,QList<Notification>)\0"
    "disconnectEvent()\0notificationUnregisterCompleted()\0"
    "retrieveNextNotifications()\0"
    "state,fault,result\0"
    "onGetNotifications(QVariant,Fault,NotificationResult)\0"
    "onGetNotificationTimeoutValue(QVariant,Fault,qint32)\0"
    "doNotificationUnregister()\0"
    "onNotificationUnregister(QVariant,Fault,OkResultCode::OkResultCodeEnum"
    ")\0"
};

const QMetaObject MoodBox::MoodBoxNotificationServer::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MoodBox__MoodBoxNotificationServer,
      qt_meta_data_MoodBox__MoodBoxNotificationServer, 0 }
};

const QMetaObject *MoodBox::MoodBoxNotificationServer::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::MoodBoxNotificationServer::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__MoodBoxNotificationServer))
        return static_cast<void*>(const_cast< MoodBoxNotificationServer*>(this));
    if (!strcmp(_clname, "MoodBoxServer"))
        return static_cast< MoodBoxServer*>(const_cast< MoodBoxNotificationServer*>(this));
    return QObject::qt_metacast(_clname);
}

int MoodBox::MoodBoxNotificationServer::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: notificationResult((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< QList<Notification>(*)>(_a[2]))); break;
        case 1: disconnectEvent(); break;
        case 2: notificationUnregisterCompleted(); break;
        case 3: retrieveNextNotifications(); break;
        case 4: onGetNotifications((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< NotificationResult(*)>(_a[3]))); break;
        case 5: onGetNotificationTimeoutValue((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< qint32(*)>(_a[3]))); break;
        case 6: doNotificationUnregister(); break;
        case 7: onNotificationUnregister((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< OkResultCode::OkResultCodeEnum(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 8;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::MoodBoxNotificationServer::notificationResult(Fault _t1, QList<Notification> _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::MoodBoxNotificationServer::disconnectEvent()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void MoodBox::MoodBoxNotificationServer::notificationUnregisterCompleted()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}
QT_END_MOC_NAMESPACE
