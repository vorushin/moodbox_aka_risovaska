/****************************************************************************
** Meta object code from reading C++ file 'authorizationdialog.h'
**
** Created: Wed Jun 24 21:01:06 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "authorizationdialog.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'authorizationdialog.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__AuthorizationRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       4,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      44,   31,   30,   30, 0x05,
     155,  134,   30,   30, 0x05,

 // slots: signature, parameters, type, tag, flags
     270,  251,   30,   30, 0x08,
     371,  251,   30,   30, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__AuthorizationRequest[] = {
    "MoodBox::AuthorizationRequest\0\0"
    "fault,result\0"
    "authorizationRequestCompleted(Fault,AuthorizationResultCode::Authoriza"
    "tionResultCodeEnum)\0"
    "fault,result,approve\0"
    "authorizationResponseCompleted(Fault,AuthorizationResultCode::Authoriz"
    "ationResultCodeEnum,bool)\0"
    "state,fault,result\0"
    "onGetAuthorizationRequestResult(QVariant,Fault,AuthorizationResultCode"
    "::AuthorizationResultCodeEnum)\0"
    "onGetAuthorizationResponseResult(QVariant,Fault,AuthorizationResultCod"
    "e::AuthorizationResultCodeEnum)\0"
};

const QMetaObject MoodBox::AuthorizationRequest::staticMetaObject = {
    { &ServerRequest::staticMetaObject, qt_meta_stringdata_MoodBox__AuthorizationRequest,
      qt_meta_data_MoodBox__AuthorizationRequest, 0 }
};

const QMetaObject *MoodBox::AuthorizationRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::AuthorizationRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__AuthorizationRequest))
        return static_cast<void*>(const_cast< AuthorizationRequest*>(this));
    return ServerRequest::qt_metacast(_clname);
}

int MoodBox::AuthorizationRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: authorizationRequestCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< AuthorizationResultCode::AuthorizationResultCodeEnum(*)>(_a[2]))); break;
        case 1: authorizationResponseCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< AuthorizationResultCode::AuthorizationResultCodeEnum(*)>(_a[2])),(*reinterpret_cast< bool(*)>(_a[3]))); break;
        case 2: onGetAuthorizationRequestResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< AuthorizationResultCode::AuthorizationResultCodeEnum(*)>(_a[3]))); break;
        case 3: onGetAuthorizationResponseResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< AuthorizationResultCode::AuthorizationResultCodeEnum(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 4;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::AuthorizationRequest::authorizationRequestCompleted(Fault _t1, AuthorizationResultCode::AuthorizationResultCodeEnum _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::AuthorizationRequest::authorizationResponseCompleted(Fault _t1, AuthorizationResultCode::AuthorizationResultCodeEnum _t2, bool _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
static const uint qt_meta_data_MoodBox__AuthorizationDialog[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       8,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      43,   30,   29,   29, 0x0a,
     159,  138,   29,   29, 0x0a,
     260,   29,   29,   29, 0x0a,
     281,   29,   29,   29, 0x08,
     303,   29,   29,   29, 0x08,
     329,   29,   29,   29, 0x08,
     355,   29,   29,   29, 0x08,
     380,   29,   29,   29, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__AuthorizationDialog[] = {
    "MoodBox::AuthorizationDialog\0\0"
    "fault,result\0"
    "onGetAuthorizationRequestCompleted(Fault,AuthorizationResultCode::Auth"
    "orizationResultCodeEnum)\0"
    "fault,result,approve\0"
    "onGetAuthorizationResponseCompleted(Fault,AuthorizationResultCode::Aut"
    "horizationResultCodeEnum,bool)\0"
    "onRequestCancelled()\0on_okButton_clicked()\0"
    "on_rejectButton_clicked()\0"
    "on_cancelButton_clicked()\0"
    "onContactAvatarClicked()\0checkMessages()\0"
};

const QMetaObject MoodBox::AuthorizationDialog::staticMetaObject = {
    { &ServerDialog::staticMetaObject, qt_meta_stringdata_MoodBox__AuthorizationDialog,
      qt_meta_data_MoodBox__AuthorizationDialog, 0 }
};

const QMetaObject *MoodBox::AuthorizationDialog::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::AuthorizationDialog::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__AuthorizationDialog))
        return static_cast<void*>(const_cast< AuthorizationDialog*>(this));
    if (!strcmp(_clname, "AuthorizationDialogClass"))
        return static_cast< AuthorizationDialogClass*>(const_cast< AuthorizationDialog*>(this));
    return ServerDialog::qt_metacast(_clname);
}

int MoodBox::AuthorizationDialog::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onGetAuthorizationRequestCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< AuthorizationResultCode::AuthorizationResultCodeEnum(*)>(_a[2]))); break;
        case 1: onGetAuthorizationResponseCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< AuthorizationResultCode::AuthorizationResultCodeEnum(*)>(_a[2])),(*reinterpret_cast< bool(*)>(_a[3]))); break;
        case 2: onRequestCancelled(); break;
        case 3: on_okButton_clicked(); break;
        case 4: on_rejectButton_clicked(); break;
        case 5: on_cancelButton_clicked(); break;
        case 6: onContactAvatarClicked(); break;
        case 7: checkMessages(); break;
        default: ;
        }
        _id -= 8;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
