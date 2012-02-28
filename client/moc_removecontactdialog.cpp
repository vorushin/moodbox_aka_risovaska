/****************************************************************************
** Meta object code from reading C++ file 'removecontactdialog.h'
**
** Created: Wed Jun 24 21:01:18 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "removecontactdialog.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'removecontactdialog.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__RemoveContactRequest[] = {

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
     121,   31,   30,   30, 0x05,

 // slots: signature, parameters, type, tag, flags
     230,  211,   30,   30, 0x08,
     318,  211,   30,   30, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__RemoveContactRequest[] = {
    "MoodBox::RemoveContactRequest\0\0"
    "fault,result\0"
    "removeFriendRequestCompleted(Fault,ContactResultCode::ContactResultCod"
    "eEnum)\0"
    "removeChannelRequestCompleted(Fault,ChangeUserChannelResult::ChangeUse"
    "rChannelResultEnum)\0"
    "state,fault,result\0"
    "onGetRemoveFriendRequestResult(QVariant,Fault,ContactResultCode::Conta"
    "ctResultCodeEnum)\0"
    "onGetRemoveChannelRequestResult(QVariant,Fault,ChangeUserChannelResult"
    "::ChangeUserChannelResultEnum)\0"
};

const QMetaObject MoodBox::RemoveContactRequest::staticMetaObject = {
    { &ServerRequest::staticMetaObject, qt_meta_stringdata_MoodBox__RemoveContactRequest,
      qt_meta_data_MoodBox__RemoveContactRequest, 0 }
};

const QMetaObject *MoodBox::RemoveContactRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::RemoveContactRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__RemoveContactRequest))
        return static_cast<void*>(const_cast< RemoveContactRequest*>(this));
    return ServerRequest::qt_metacast(_clname);
}

int MoodBox::RemoveContactRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: removeFriendRequestCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< ContactResultCode::ContactResultCodeEnum(*)>(_a[2]))); break;
        case 1: removeChannelRequestCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< ChangeUserChannelResult::ChangeUserChannelResultEnum(*)>(_a[2]))); break;
        case 2: onGetRemoveFriendRequestResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< ContactResultCode::ContactResultCodeEnum(*)>(_a[3]))); break;
        case 3: onGetRemoveChannelRequestResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< ChangeUserChannelResult::ChangeUserChannelResultEnum(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 4;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::RemoveContactRequest::removeFriendRequestCompleted(Fault _t1, ContactResultCode::ContactResultCodeEnum _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::RemoveContactRequest::removeChannelRequestCompleted(Fault _t1, ChangeUserChannelResult::ChangeUserChannelResultEnum _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
static const uint qt_meta_data_MoodBox__RemoveContactDialog[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       4,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      43,   30,   29,   29, 0x0a,
     115,   30,   29,   29, 0x0a,
     200,   29,   29,   29, 0x0a,
     221,   29,   29,   29, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__RemoveContactDialog[] = {
    "MoodBox::RemoveContactDialog\0\0"
    "fault,result\0"
    "onGetRemoveFriendResult(Fault,ContactResultCode::ContactResultCodeEnum"
    ")\0"
    "onGetRemoveChannelResult(Fault,ChangeUserChannelResult::ChangeUserChan"
    "nelResultEnum)\0"
    "onRequestCancelled()\0on_removeButton_clicked()\0"
};

const QMetaObject MoodBox::RemoveContactDialog::staticMetaObject = {
    { &ServerDialog::staticMetaObject, qt_meta_stringdata_MoodBox__RemoveContactDialog,
      qt_meta_data_MoodBox__RemoveContactDialog, 0 }
};

const QMetaObject *MoodBox::RemoveContactDialog::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::RemoveContactDialog::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__RemoveContactDialog))
        return static_cast<void*>(const_cast< RemoveContactDialog*>(this));
    if (!strcmp(_clname, "RemoveContactDialogClass"))
        return static_cast< RemoveContactDialogClass*>(const_cast< RemoveContactDialog*>(this));
    return ServerDialog::qt_metacast(_clname);
}

int MoodBox::RemoveContactDialog::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onGetRemoveFriendResult((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< ContactResultCode::ContactResultCodeEnum(*)>(_a[2]))); break;
        case 1: onGetRemoveChannelResult((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< ChangeUserChannelResult::ChangeUserChannelResultEnum(*)>(_a[2]))); break;
        case 2: onRequestCancelled(); break;
        case 3: on_removeButton_clicked(); break;
        default: ;
        }
        _id -= 4;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
