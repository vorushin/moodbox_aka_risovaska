/****************************************************************************
** Meta object code from reading C++ file 'channellistitem.h'
**
** Created: Wed Jun 24 21:01:08 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "channellistitem.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'channellistitem.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__AddChannelRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      41,   28,   27,   27, 0x05,

 // slots: signature, parameters, type, tag, flags
     147,  128,   27,   27, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__AddChannelRequest[] = {
    "MoodBox::AddChannelRequest\0\0fault,result\0"
    "addChannelRequestCompleted(Fault,ChangeUserChannelResult::ChangeUserCh"
    "annelResultEnum)\0"
    "state,fault,result\0"
    "onGetAddChannelRequestResult(QVariant,Fault,ChangeUserChannelResult::C"
    "hangeUserChannelResultEnum)\0"
};

const QMetaObject MoodBox::AddChannelRequest::staticMetaObject = {
    { &ServerRequest::staticMetaObject, qt_meta_stringdata_MoodBox__AddChannelRequest,
      qt_meta_data_MoodBox__AddChannelRequest, 0 }
};

const QMetaObject *MoodBox::AddChannelRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::AddChannelRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__AddChannelRequest))
        return static_cast<void*>(const_cast< AddChannelRequest*>(this));
    return ServerRequest::qt_metacast(_clname);
}

int MoodBox::AddChannelRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: addChannelRequestCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< ChangeUserChannelResult::ChangeUserChannelResultEnum(*)>(_a[2]))); break;
        case 1: onGetAddChannelRequestResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< ChangeUserChannelResult::ChangeUserChannelResultEnum(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::AddChannelRequest::addChannelRequestCompleted(Fault _t1, ChangeUserChannelResult::ChangeUserChannelResultEnum _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__RemoveChannelRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      44,   31,   30,   30, 0x05,

 // slots: signature, parameters, type, tag, flags
     153,  134,   30,   30, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__RemoveChannelRequest[] = {
    "MoodBox::RemoveChannelRequest\0\0"
    "fault,result\0"
    "removeChannelRequestCompleted(Fault,ChangeUserChannelResult::ChangeUse"
    "rChannelResultEnum)\0"
    "state,fault,result\0"
    "onGetRemoveChannelRequestResult(QVariant,Fault,ChangeUserChannelResult"
    "::ChangeUserChannelResultEnum)\0"
};

const QMetaObject MoodBox::RemoveChannelRequest::staticMetaObject = {
    { &ServerRequest::staticMetaObject, qt_meta_stringdata_MoodBox__RemoveChannelRequest,
      qt_meta_data_MoodBox__RemoveChannelRequest, 0 }
};

const QMetaObject *MoodBox::RemoveChannelRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::RemoveChannelRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__RemoveChannelRequest))
        return static_cast<void*>(const_cast< RemoveChannelRequest*>(this));
    return ServerRequest::qt_metacast(_clname);
}

int MoodBox::RemoveChannelRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: removeChannelRequestCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< ChangeUserChannelResult::ChangeUserChannelResultEnum(*)>(_a[2]))); break;
        case 1: onGetRemoveChannelRequestResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< ChangeUserChannelResult::ChangeUserChannelResultEnum(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::RemoveChannelRequest::removeChannelRequestCompleted(Fault _t1, ChangeUserChannelResult::ChangeUserChannelResultEnum _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__ChannelListItem[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      39,   26,   25,   25, 0x0a,
     121,   26,   25,   25, 0x0a,
     206,   25,   25,   25, 0x0a,
     231,  227,   25,   25, 0x08,
     259,   25,   25,   25, 0x08,
     289,   25,   25,   25, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ChannelListItem[] = {
    "MoodBox::ChannelListItem\0\0fault,result\0"
    "onGetAddChannelResult(Fault,ChangeUserChannelResult::ChangeUserChannel"
    "ResultEnum)\0"
    "onGetRemoveChannelResult(Fault,ChangeUserChannelResult::ChangeUserChan"
    "nelResultEnum)\0"
    "onRequestCancelled()\0key\0"
    "onNewPictureLoaded(QString)\0"
    "on_addChannelButton_clicked()\0"
    "on_removeChannelButton_clicked()\0"
};

const QMetaObject MoodBox::ChannelListItem::staticMetaObject = {
    { &ServerFrame::staticMetaObject, qt_meta_stringdata_MoodBox__ChannelListItem,
      qt_meta_data_MoodBox__ChannelListItem, 0 }
};

const QMetaObject *MoodBox::ChannelListItem::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ChannelListItem::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ChannelListItem))
        return static_cast<void*>(const_cast< ChannelListItem*>(this));
    if (!strcmp(_clname, "ChannelListItemClass"))
        return static_cast< ChannelListItemClass*>(const_cast< ChannelListItem*>(this));
    return ServerFrame::qt_metacast(_clname);
}

int MoodBox::ChannelListItem::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerFrame::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onGetAddChannelResult((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< ChangeUserChannelResult::ChangeUserChannelResultEnum(*)>(_a[2]))); break;
        case 1: onGetRemoveChannelResult((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< ChangeUserChannelResult::ChangeUserChannelResultEnum(*)>(_a[2]))); break;
        case 2: onRequestCancelled(); break;
        case 3: onNewPictureLoaded((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 4: on_addChannelButton_clicked(); break;
        case 5: on_removeChannelButton_clicked(); break;
        default: ;
        }
        _id -= 6;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
