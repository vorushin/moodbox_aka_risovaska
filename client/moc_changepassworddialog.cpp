/****************************************************************************
** Meta object code from reading C++ file 'changepassworddialog.h'
**
** Created: Wed Jun 24 21:01:08 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "changepassworddialog.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'changepassworddialog.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__ChangePasswordRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      45,   32,   31,   31, 0x05,

 // slots: signature, parameters, type, tag, flags
     143,  124,   31,   31, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ChangePasswordRequest[] = {
    "MoodBox::ChangePasswordRequest\0\0"
    "fault,result\0"
    "changePasswordRequestCompleted(Fault,AccountResultCode::AccountResultC"
    "odeEnum)\0"
    "state,fault,result\0"
    "onGetChangePasswordRequestResult(QVariant,Fault,AccountResultCode::Acc"
    "ountResultCodeEnum)\0"
};

const QMetaObject MoodBox::ChangePasswordRequest::staticMetaObject = {
    { &ServerRequest::staticMetaObject, qt_meta_stringdata_MoodBox__ChangePasswordRequest,
      qt_meta_data_MoodBox__ChangePasswordRequest, 0 }
};

const QMetaObject *MoodBox::ChangePasswordRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ChangePasswordRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ChangePasswordRequest))
        return static_cast<void*>(const_cast< ChangePasswordRequest*>(this));
    return ServerRequest::qt_metacast(_clname);
}

int MoodBox::ChangePasswordRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: changePasswordRequestCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< AccountResultCode::AccountResultCodeEnum(*)>(_a[2]))); break;
        case 1: onGetChangePasswordRequestResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< AccountResultCode::AccountResultCodeEnum(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::ChangePasswordRequest::changePasswordRequestCompleted(Fault _t1, AccountResultCode::AccountResultCodeEnum _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__ChangePasswordDialog[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      44,   31,   30,   30, 0x0a,
     117,   30,   30,   30, 0x0a,
     138,   30,   30,   30, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ChangePasswordDialog[] = {
    "MoodBox::ChangePasswordDialog\0\0"
    "fault,result\0"
    "onUpdatePasswordResponse(Fault,AccountResultCode::AccountResultCodeEnu"
    "m)\0"
    "onRequestCancelled()\0on_okButton_clicked()\0"
};

const QMetaObject MoodBox::ChangePasswordDialog::staticMetaObject = {
    { &ServerDialog::staticMetaObject, qt_meta_stringdata_MoodBox__ChangePasswordDialog,
      qt_meta_data_MoodBox__ChangePasswordDialog, 0 }
};

const QMetaObject *MoodBox::ChangePasswordDialog::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ChangePasswordDialog::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ChangePasswordDialog))
        return static_cast<void*>(const_cast< ChangePasswordDialog*>(this));
    if (!strcmp(_clname, "ChangePasswordDialogClass"))
        return static_cast< ChangePasswordDialogClass*>(const_cast< ChangePasswordDialog*>(this));
    return ServerDialog::qt_metacast(_clname);
}

int MoodBox::ChangePasswordDialog::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onUpdatePasswordResponse((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< AccountResultCode::AccountResultCodeEnum(*)>(_a[2]))); break;
        case 1: onRequestCancelled(); break;
        case 2: on_okButton_clicked(); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
