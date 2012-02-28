/****************************************************************************
** Meta object code from reading C++ file 'servercommandreceiver.h'
**
** Created: Wed Jun 24 21:01:18 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "servercommandreceiver.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'servercommandreceiver.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__GetNextCommandsRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      48,   33,   32,   32, 0x05,

 // slots: signature, parameters, type, tag, flags
      86,   32,   32,   32, 0x09,
     122,  103,   32,   32, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__GetNextCommandsRequest[] = {
    "MoodBox::GetNextCommandsRequest\0\0"
    "commandPackage\0gotNextCommandPackage(CommandPackage)\0"
    "onTimerTimeout()\0state,fault,result\0"
    "onGetNextCommandsResult(QVariant,Fault,CommandPackage)\0"
};

const QMetaObject MoodBox::GetNextCommandsRequest::staticMetaObject = {
    { &RandomRetryTimerObject::staticMetaObject, qt_meta_stringdata_MoodBox__GetNextCommandsRequest,
      qt_meta_data_MoodBox__GetNextCommandsRequest, 0 }
};

const QMetaObject *MoodBox::GetNextCommandsRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::GetNextCommandsRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__GetNextCommandsRequest))
        return static_cast<void*>(const_cast< GetNextCommandsRequest*>(this));
    return RandomRetryTimerObject::qt_metacast(_clname);
}

int MoodBox::GetNextCommandsRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = RandomRetryTimerObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: gotNextCommandPackage((*reinterpret_cast< const CommandPackage(*)>(_a[1]))); break;
        case 1: onTimerTimeout(); break;
        case 2: onGetNextCommandsResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< CommandPackage(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::GetNextCommandsRequest::gotNextCommandPackage(const CommandPackage & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__ServerCommandsReceiver[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      47,   33,   32,   32, 0x08,
     102,   87,   32,   32, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ServerCommandsReceiver[] = {
    "MoodBox::ServerCommandsReceiver\0\0"
    "notifications\0onGetNotifications(QList<Notification>)\0"
    "commandPackage\0onGetNextCommands(CommandPackage)\0"
};

const QMetaObject MoodBox::ServerCommandsReceiver::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MoodBox__ServerCommandsReceiver,
      qt_meta_data_MoodBox__ServerCommandsReceiver, 0 }
};

const QMetaObject *MoodBox::ServerCommandsReceiver::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ServerCommandsReceiver::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ServerCommandsReceiver))
        return static_cast<void*>(const_cast< ServerCommandsReceiver*>(this));
    return QObject::qt_metacast(_clname);
}

int MoodBox::ServerCommandsReceiver::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onGetNotifications((*reinterpret_cast< QList<Notification>(*)>(_a[1]))); break;
        case 1: onGetNextCommands((*reinterpret_cast< const CommandPackage(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
