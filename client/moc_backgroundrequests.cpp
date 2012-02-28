/****************************************************************************
** Meta object code from reading C++ file 'backgroundrequests.h'
**
** Created: Wed Jun 24 21:01:07 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "backgroundrequests.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'backgroundrequests.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__InfoManagerBackgroundRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      39,   38,   38,   38, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__InfoManagerBackgroundRequest[] = {
    "MoodBox::InfoManagerBackgroundRequest\0"
    "\0onTimerTimeout()\0"
};

const QMetaObject MoodBox::InfoManagerBackgroundRequest::staticMetaObject = {
    { &RandomRetryTimerObject::staticMetaObject, qt_meta_stringdata_MoodBox__InfoManagerBackgroundRequest,
      qt_meta_data_MoodBox__InfoManagerBackgroundRequest, 0 }
};

const QMetaObject *MoodBox::InfoManagerBackgroundRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::InfoManagerBackgroundRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__InfoManagerBackgroundRequest))
        return static_cast<void*>(const_cast< InfoManagerBackgroundRequest*>(this));
    return RandomRetryTimerObject::qt_metacast(_clname);
}

int MoodBox::InfoManagerBackgroundRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = RandomRetryTimerObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onTimerTimeout(); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
static const uint qt_meta_data_MoodBox__GetMyAccountRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      50,   30,   29,   29, 0x05,

 // slots: signature, parameters, type, tag, flags
      97,   30,   29,   29, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__GetMyAccountRequest[] = {
    "MoodBox::GetMyAccountRequest\0\0"
    "state,fault,account\0"
    "gotMyAccountResult(QVariant,Fault,UserAccount)\0"
    "onGetMyAccountResult(QVariant,Fault,UserAccount)\0"
};

const QMetaObject MoodBox::GetMyAccountRequest::staticMetaObject = {
    { &InfoManagerBackgroundRequest::staticMetaObject, qt_meta_stringdata_MoodBox__GetMyAccountRequest,
      qt_meta_data_MoodBox__GetMyAccountRequest, 0 }
};

const QMetaObject *MoodBox::GetMyAccountRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::GetMyAccountRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__GetMyAccountRequest))
        return static_cast<void*>(const_cast< GetMyAccountRequest*>(this));
    return InfoManagerBackgroundRequest::qt_metacast(_clname);
}

int MoodBox::GetMyAccountRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = InfoManagerBackgroundRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: gotMyAccountResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< UserAccount(*)>(_a[3]))); break;
        case 1: onGetMyAccountResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< UserAccount(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::GetMyAccountRequest::gotMyAccountResult(QVariant _t1, Fault _t2, UserAccount _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__GetContactsRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      48,   29,   28,   28, 0x05,

 // slots: signature, parameters, type, tag, flags
     101,   29,   28,   28, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__GetContactsRequest[] = {
    "MoodBox::GetContactsRequest\0\0"
    "state,fault,result\0"
    "gotContactsResult(QVariant,Fault,QList<ContactInfo>)\0"
    "onGetContactsResult(QVariant,Fault,QList<ContactInfo>)\0"
};

const QMetaObject MoodBox::GetContactsRequest::staticMetaObject = {
    { &InfoManagerBackgroundRequest::staticMetaObject, qt_meta_stringdata_MoodBox__GetContactsRequest,
      qt_meta_data_MoodBox__GetContactsRequest, 0 }
};

const QMetaObject *MoodBox::GetContactsRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::GetContactsRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__GetContactsRequest))
        return static_cast<void*>(const_cast< GetContactsRequest*>(this));
    return InfoManagerBackgroundRequest::qt_metacast(_clname);
}

int MoodBox::GetContactsRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = InfoManagerBackgroundRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: gotContactsResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< QList<ContactInfo>(*)>(_a[3]))); break;
        case 1: onGetContactsResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< QList<ContactInfo>(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::GetContactsRequest::gotContactsResult(QVariant _t1, Fault _t2, QList<ContactInfo> _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__GetContactRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      47,   28,   27,   27, 0x05,

 // slots: signature, parameters, type, tag, flags
      92,   28,   27,   27, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__GetContactRequest[] = {
    "MoodBox::GetContactRequest\0\0"
    "state,fault,result\0"
    "gotContactResult(QVariant,Fault,ContactInfo)\0"
    "onGetContactResult(QVariant,Fault,ContactInfo)\0"
};

const QMetaObject MoodBox::GetContactRequest::staticMetaObject = {
    { &InfoManagerBackgroundRequest::staticMetaObject, qt_meta_stringdata_MoodBox__GetContactRequest,
      qt_meta_data_MoodBox__GetContactRequest, 0 }
};

const QMetaObject *MoodBox::GetContactRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::GetContactRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__GetContactRequest))
        return static_cast<void*>(const_cast< GetContactRequest*>(this));
    return InfoManagerBackgroundRequest::qt_metacast(_clname);
}

int MoodBox::GetContactRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = InfoManagerBackgroundRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: gotContactResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< ContactInfo(*)>(_a[3]))); break;
        case 1: onGetContactResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< ContactInfo(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::GetContactRequest::gotContactResult(QVariant _t1, Fault _t2, ContactInfo _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__GetStatusRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      46,   27,   26,   26, 0x05,

 // slots: signature, parameters, type, tag, flags
     105,   27,   26,   26, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__GetStatusRequest[] = {
    "MoodBox::GetStatusRequest\0\0"
    "state,fault,result\0"
    "gotStatusResult(QVariant,Fault,UserStatus::UserStatusEnum)\0"
    "onGetStatusResult(QVariant,Fault,UserStatus::UserStatusEnum)\0"
};

const QMetaObject MoodBox::GetStatusRequest::staticMetaObject = {
    { &InfoManagerBackgroundRequest::staticMetaObject, qt_meta_stringdata_MoodBox__GetStatusRequest,
      qt_meta_data_MoodBox__GetStatusRequest, 0 }
};

const QMetaObject *MoodBox::GetStatusRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::GetStatusRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__GetStatusRequest))
        return static_cast<void*>(const_cast< GetStatusRequest*>(this));
    return InfoManagerBackgroundRequest::qt_metacast(_clname);
}

int MoodBox::GetStatusRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = InfoManagerBackgroundRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: gotStatusResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< UserStatus::UserStatusEnum(*)>(_a[3]))); break;
        case 1: onGetStatusResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< UserStatus::UserStatusEnum(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::GetStatusRequest::gotStatusResult(QVariant _t1, Fault _t2, UserStatus::UserStatusEnum _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__GetAuthorizationRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      53,   34,   33,   33, 0x05,

 // slots: signature, parameters, type, tag, flags
     106,   34,   33,   33, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__GetAuthorizationRequest[] = {
    "MoodBox::GetAuthorizationRequest\0\0"
    "state,fault,result\0"
    "gotAuthorizationResult(QVariant,Fault,Authorization)\0"
    "onGetAuthorizationResult(QVariant,Fault,Authorization)\0"
};

const QMetaObject MoodBox::GetAuthorizationRequest::staticMetaObject = {
    { &InfoManagerBackgroundRequest::staticMetaObject, qt_meta_stringdata_MoodBox__GetAuthorizationRequest,
      qt_meta_data_MoodBox__GetAuthorizationRequest, 0 }
};

const QMetaObject *MoodBox::GetAuthorizationRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::GetAuthorizationRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__GetAuthorizationRequest))
        return static_cast<void*>(const_cast< GetAuthorizationRequest*>(this));
    return InfoManagerBackgroundRequest::qt_metacast(_clname);
}

int MoodBox::GetAuthorizationRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = InfoManagerBackgroundRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: gotAuthorizationResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< Authorization(*)>(_a[3]))); break;
        case 1: onGetAuthorizationResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< Authorization(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::GetAuthorizationRequest::gotAuthorizationResult(QVariant _t1, Fault _t2, Authorization _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
