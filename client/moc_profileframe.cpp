/****************************************************************************
** Meta object code from reading C++ file 'profileframe.h'
**
** Created: Wed Jun 24 21:01:17 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "profileframe.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'profileframe.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__UserAccountUpdateRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      48,   35,   34,   34, 0x05,

 // slots: signature, parameters, type, tag, flags
     138,  119,   34,   34, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__UserAccountUpdateRequest[] = {
    "MoodBox::UserAccountUpdateRequest\0\0"
    "fault,result\0"
    "accountUpdateCompleted(Fault,AccountResultCode::AccountResultCodeEnum)\0"
    "state,fault,result\0"
    "onUpdateAccountResult(QVariant,Fault,AccountResultCode::AccountResultC"
    "odeEnum)\0"
};

const QMetaObject MoodBox::UserAccountUpdateRequest::staticMetaObject = {
    { &ServerRequest::staticMetaObject, qt_meta_stringdata_MoodBox__UserAccountUpdateRequest,
      qt_meta_data_MoodBox__UserAccountUpdateRequest, 0 }
};

const QMetaObject *MoodBox::UserAccountUpdateRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::UserAccountUpdateRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__UserAccountUpdateRequest))
        return static_cast<void*>(const_cast< UserAccountUpdateRequest*>(this));
    return ServerRequest::qt_metacast(_clname);
}

int MoodBox::UserAccountUpdateRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: accountUpdateCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< AccountResultCode::AccountResultCodeEnum(*)>(_a[2]))); break;
        case 1: onUpdateAccountResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< AccountResultCode::AccountResultCodeEnum(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::UserAccountUpdateRequest::accountUpdateCompleted(Fault _t1, AccountResultCode::AccountResultCodeEnum _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__ProfileFrame[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       7,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      36,   23,   22,   22, 0x0a,
     109,   22,   22,   22, 0x0a,
     130,   22,   22,   22, 0x08,
     152,   22,   22,   22, 0x08,
     180,   22,   22,   22, 0x08,
     212,   22,   22,   22, 0x08,
     228,   22,   22,   22, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ProfileFrame[] = {
    "MoodBox::ProfileFrame\0\0fault,result\0"
    "onAccountUpdateCompleted(Fault,AccountResultCode::AccountResultCodeEnu"
    "m)\0"
    "onRequestCancelled()\0onMonthComboChanged()\0"
    "onUserAvatarButtonClicked()\0"
    "onChangePasswordButtonClicked()\0"
    "clearBirthday()\0profileChanged()\0"
};

const QMetaObject MoodBox::ProfileFrame::staticMetaObject = {
    { &SetupDialogFrame::staticMetaObject, qt_meta_stringdata_MoodBox__ProfileFrame,
      qt_meta_data_MoodBox__ProfileFrame, 0 }
};

const QMetaObject *MoodBox::ProfileFrame::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ProfileFrame::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ProfileFrame))
        return static_cast<void*>(const_cast< ProfileFrame*>(this));
    if (!strcmp(_clname, "ProfileFrameClass"))
        return static_cast< ProfileFrameClass*>(const_cast< ProfileFrame*>(this));
    return SetupDialogFrame::qt_metacast(_clname);
}

int MoodBox::ProfileFrame::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = SetupDialogFrame::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onAccountUpdateCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< AccountResultCode::AccountResultCodeEnum(*)>(_a[2]))); break;
        case 1: onRequestCancelled(); break;
        case 2: onMonthComboChanged(); break;
        case 3: onUserAvatarButtonClicked(); break;
        case 4: onChangePasswordButtonClicked(); break;
        case 5: clearBirthday(); break;
        case 6: profileChanged(); break;
        default: ;
        }
        _id -= 7;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
