/****************************************************************************
** Meta object code from reading C++ file 'contactinfodialog.h'
**
** Created: Wed Jun 24 21:01:09 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "contactinfodialog.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'contactinfodialog.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__ContactInfoRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       4,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      44,   29,   28,   28, 0x05,
     103,   85,   28,   28, 0x05,

 // slots: signature, parameters, type, tag, flags
     173,  152,   28,   28, 0x08,
     246,  222,   28,   28, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ContactInfoRequest[] = {
    "MoodBox::ContactInfoRequest\0\0"
    "fault,userInfo\0userInfoRequestCompleted(Fault,UserInfo)\0"
    "fault,channelInfo\0"
    "channelInfoRequestCompleted(Fault,ChannelResult)\0"
    "state,fault,userInfo\0"
    "onUserInfoRequestResult(QVariant,Fault,UserInfo)\0"
    "state,fault,channelInfo\0"
    "onChannelInfoRequestResult(QVariant,Fault,ChannelResult)\0"
};

const QMetaObject MoodBox::ContactInfoRequest::staticMetaObject = {
    { &ServerRequest::staticMetaObject, qt_meta_stringdata_MoodBox__ContactInfoRequest,
      qt_meta_data_MoodBox__ContactInfoRequest, 0 }
};

const QMetaObject *MoodBox::ContactInfoRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ContactInfoRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ContactInfoRequest))
        return static_cast<void*>(const_cast< ContactInfoRequest*>(this));
    return ServerRequest::qt_metacast(_clname);
}

int MoodBox::ContactInfoRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: userInfoRequestCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< UserInfo(*)>(_a[2]))); break;
        case 1: channelInfoRequestCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< ChannelResult(*)>(_a[2]))); break;
        case 2: onUserInfoRequestResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< UserInfo(*)>(_a[3]))); break;
        case 3: onChannelInfoRequestResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< ChannelResult(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 4;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::ContactInfoRequest::userInfoRequestCompleted(Fault _t1, UserInfo _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::ContactInfoRequest::channelInfoRequestCompleted(Fault _t1, ChannelResult _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
static const uint qt_meta_data_MoodBox__ContactInfoDialog[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       7,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      43,   28,   27,   27, 0x0a,
     104,   86,   27,   27, 0x0a,
     155,   27,   27,   27, 0x0a,
     176,   27,   27,   27, 0x08,
     204,   27,   27,   27, 0x08,
     239,  235,   27,   27, 0x08,
     267,   27,   27,   27, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ContactInfoDialog[] = {
    "MoodBox::ContactInfoDialog\0\0fault,userInfo\0"
    "onUserInfoRequestCompleted(Fault,UserInfo)\0"
    "fault,channelInfo\0"
    "onChannelInfoRequestCompleted(Fault,ChannelResult)\0"
    "onRequestCancelled()\0on_nameLinkButton_clicked()\0"
    "on_addAsFriendButton_clicked()\0key\0"
    "onNewPictureLoaded(QString)\0loadPicture()\0"
};

const QMetaObject MoodBox::ContactInfoDialog::staticMetaObject = {
    { &ServerDialog::staticMetaObject, qt_meta_stringdata_MoodBox__ContactInfoDialog,
      qt_meta_data_MoodBox__ContactInfoDialog, 0 }
};

const QMetaObject *MoodBox::ContactInfoDialog::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ContactInfoDialog::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ContactInfoDialog))
        return static_cast<void*>(const_cast< ContactInfoDialog*>(this));
    if (!strcmp(_clname, "ContactInfoDialogClass"))
        return static_cast< ContactInfoDialogClass*>(const_cast< ContactInfoDialog*>(this));
    return ServerDialog::qt_metacast(_clname);
}

int MoodBox::ContactInfoDialog::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onUserInfoRequestCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< UserInfo(*)>(_a[2]))); break;
        case 1: onChannelInfoRequestCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< ChannelResult(*)>(_a[2]))); break;
        case 2: onRequestCancelled(); break;
        case 3: on_nameLinkButton_clicked(); break;
        case 4: on_addAsFriendButton_clicked(); break;
        case 5: onNewPictureLoaded((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 6: loadPicture(); break;
        default: ;
        }
        _id -= 7;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
