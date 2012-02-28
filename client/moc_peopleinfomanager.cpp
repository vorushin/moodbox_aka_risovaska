/****************************************************************************
** Meta object code from reading C++ file 'peopleinfomanager.h'
**
** Created: Wed Jun 24 21:01:16 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "peopleinfomanager.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'peopleinfomanager.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__PeopleInfoManager[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      18,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      28,   27,   27,   27, 0x05,
      53,   46,   27,   27, 0x05,
      99,   27,   27,   27, 0x05,
     120,   27,   27,   27, 0x05,
     151,  141,   27,   27, 0x05,
     229,  207,   27,   27, 0x05,
     317,  308,   27,   27, 0x05,
     361,  353,   27,   27, 0x05,
     399,  396,   27,   27, 0x05,
     429,  396,   27,   27, 0x05,
     456,   27,   27,   27, 0x05,

 // slots: signature, parameters, type, tag, flags
     497,  477,   27,   27, 0x0a,
     565,  546,   27,   27, 0x0a,
     620,  546,   27,   27, 0x0a,
     667,  546,   27,   27, 0x0a,
     728,  546,   27,   27, 0x0a,
     786,  546,   27,   27, 0x0a,
     855,  841,   27,   27, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__PeopleInfoManager[] = {
    "MoodBox::PeopleInfoManager\0\0"
    "isOnlineChanged()\0status\0"
    "userStatusChanged(UserStatus::UserStatusEnum)\0"
    "userPictureChanged()\0userAccountUpdated()\0"
    "id,status\0"
    "contactStatusChanged(qint32,UserStatus::UserStatusEnum)\0"
    "id,authorizationState\0"
    "contactAuthorizationChanged(qint32,AuthorizationState::AuthorizationSt"
    "ateEnum)\0"
    "id,motto\0contactMottoChanged(qint32,QString)\0"
    "id,name\0contactNameChanged(qint32,QString)\0"
    "id\0contactPictureChanged(qint32)\0"
    "contactInfoChanged(qint32)\0"
    "contactListChanged()\0state,fault,account\0"
    "onGetMyAccountResult(QVariant,Fault,UserAccount)\0"
    "state,fault,result\0"
    "onGetContactsResult(QVariant,Fault,QList<ContactInfo>)\0"
    "onGetContactResult(QVariant,Fault,ContactInfo)\0"
    "onGetStatusResult(QVariant,Fault,UserStatus::UserStatusEnum)\0"
    "onGetAuthorizingContactResult(QVariant,Fault,ContactInfo)\0"
    "onGetAuthorizationResult(QVariant,Fault,Authorization)\0"
    "notifications\0onGetNotifications(QList<Notification>)\0"
};

const QMetaObject MoodBox::PeopleInfoManager::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MoodBox__PeopleInfoManager,
      qt_meta_data_MoodBox__PeopleInfoManager, 0 }
};

const QMetaObject *MoodBox::PeopleInfoManager::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::PeopleInfoManager::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__PeopleInfoManager))
        return static_cast<void*>(const_cast< PeopleInfoManager*>(this));
    return QObject::qt_metacast(_clname);
}

int MoodBox::PeopleInfoManager::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: isOnlineChanged(); break;
        case 1: userStatusChanged((*reinterpret_cast< UserStatus::UserStatusEnum(*)>(_a[1]))); break;
        case 2: userPictureChanged(); break;
        case 3: userAccountUpdated(); break;
        case 4: contactStatusChanged((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< UserStatus::UserStatusEnum(*)>(_a[2]))); break;
        case 5: contactAuthorizationChanged((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< AuthorizationState::AuthorizationStateEnum(*)>(_a[2]))); break;
        case 6: contactMottoChanged((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const QString(*)>(_a[2]))); break;
        case 7: contactNameChanged((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const QString(*)>(_a[2]))); break;
        case 8: contactPictureChanged((*reinterpret_cast< qint32(*)>(_a[1]))); break;
        case 9: contactInfoChanged((*reinterpret_cast< qint32(*)>(_a[1]))); break;
        case 10: contactListChanged(); break;
        case 11: onGetMyAccountResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< UserAccount(*)>(_a[3]))); break;
        case 12: onGetContactsResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< QList<ContactInfo>(*)>(_a[3]))); break;
        case 13: onGetContactResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< ContactInfo(*)>(_a[3]))); break;
        case 14: onGetStatusResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< UserStatus::UserStatusEnum(*)>(_a[3]))); break;
        case 15: onGetAuthorizingContactResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< ContactInfo(*)>(_a[3]))); break;
        case 16: onGetAuthorizationResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< Authorization(*)>(_a[3]))); break;
        case 17: onGetNotifications((*reinterpret_cast< QList<Notification>(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 18;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::PeopleInfoManager::isOnlineChanged()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void MoodBox::PeopleInfoManager::userStatusChanged(UserStatus::UserStatusEnum _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void MoodBox::PeopleInfoManager::userPictureChanged()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}

// SIGNAL 3
void MoodBox::PeopleInfoManager::userAccountUpdated()
{
    QMetaObject::activate(this, &staticMetaObject, 3, 0);
}

// SIGNAL 4
void MoodBox::PeopleInfoManager::contactStatusChanged(qint32 _t1, UserStatus::UserStatusEnum _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 4, _a);
}

// SIGNAL 5
void MoodBox::PeopleInfoManager::contactAuthorizationChanged(qint32 _t1, AuthorizationState::AuthorizationStateEnum _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 5, _a);
}

// SIGNAL 6
void MoodBox::PeopleInfoManager::contactMottoChanged(qint32 _t1, const QString & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 6, _a);
}

// SIGNAL 7
void MoodBox::PeopleInfoManager::contactNameChanged(qint32 _t1, const QString & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 7, _a);
}

// SIGNAL 8
void MoodBox::PeopleInfoManager::contactPictureChanged(qint32 _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 8, _a);
}

// SIGNAL 9
void MoodBox::PeopleInfoManager::contactInfoChanged(qint32 _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 9, _a);
}

// SIGNAL 10
void MoodBox::PeopleInfoManager::contactListChanged()
{
    QMetaObject::activate(this, &staticMetaObject, 10, 0);
}
QT_END_MOC_NAMESPACE
