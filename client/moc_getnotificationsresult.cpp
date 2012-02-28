/****************************************************************************
** Meta object code from reading C++ file 'getnotificationsresult.h'
**
** Created: Wed Jun 24 21:01:23 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "ModelGenerated/getnotificationsresult.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'getnotificationsresult.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__GetNotificationsResultCallbackCaller[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      66,   47,   46,   46, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__GetNotificationsResultCallbackCaller[] = {
    "MoodBox::GetNotificationsResultCallbackCaller\0"
    "\0state,fault,result\0"
    "callbackSignal(QVariant,Fault,NotificationResult)\0"
};

const QMetaObject MoodBox::GetNotificationsResultCallbackCaller::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MoodBox__GetNotificationsResultCallbackCaller,
      qt_meta_data_MoodBox__GetNotificationsResultCallbackCaller, 0 }
};

const QMetaObject *MoodBox::GetNotificationsResultCallbackCaller::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::GetNotificationsResultCallbackCaller::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__GetNotificationsResultCallbackCaller))
        return static_cast<void*>(const_cast< GetNotificationsResultCallbackCaller*>(this));
    return QObject::qt_metacast(_clname);
}

int MoodBox::GetNotificationsResultCallbackCaller::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: callbackSignal((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< NotificationResult(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::GetNotificationsResultCallbackCaller::callbackSignal(QVariant _t1, Fault _t2, NotificationResult _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
