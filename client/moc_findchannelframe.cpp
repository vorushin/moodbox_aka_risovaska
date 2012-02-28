/****************************************************************************
** Meta object code from reading C++ file 'findchannelframe.h'
**
** Created: Wed Jun 24 21:01:10 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "findchannelframe.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'findchannelframe.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__SearchChannelRequest[] = {

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
     106,   87,   30,   30, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__SearchChannelRequest[] = {
    "MoodBox::SearchChannelRequest\0\0"
    "fault,result\0searchCompleted(Fault,ChannelSearchResult)\0"
    "state,fault,result\0"
    "onSearchResult(QVariant,Fault,ChannelSearchResult)\0"
};

const QMetaObject MoodBox::SearchChannelRequest::staticMetaObject = {
    { &ServerRequest::staticMetaObject, qt_meta_stringdata_MoodBox__SearchChannelRequest,
      qt_meta_data_MoodBox__SearchChannelRequest, 0 }
};

const QMetaObject *MoodBox::SearchChannelRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::SearchChannelRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__SearchChannelRequest))
        return static_cast<void*>(const_cast< SearchChannelRequest*>(this));
    return ServerRequest::qt_metacast(_clname);
}

int MoodBox::SearchChannelRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: searchCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< ChannelSearchResult(*)>(_a[2]))); break;
        case 1: onSearchResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< ChannelSearchResult(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::SearchChannelRequest::searchCompleted(Fault _t1, ChannelSearchResult _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__FindChannelFrame[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      40,   27,   26,   26, 0x0a,
      84,   26,   26,   26, 0x0a,
     105,   26,   26,   26, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__FindChannelFrame[] = {
    "MoodBox::FindChannelFrame\0\0fault,result\0"
    "onSearchContacts(Fault,ChannelSearchResult)\0"
    "onRequestCancelled()\0onContactListChanged()\0"
};

const QMetaObject MoodBox::FindChannelFrame::staticMetaObject = {
    { &ServerFrame::staticMetaObject, qt_meta_stringdata_MoodBox__FindChannelFrame,
      qt_meta_data_MoodBox__FindChannelFrame, 0 }
};

const QMetaObject *MoodBox::FindChannelFrame::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::FindChannelFrame::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__FindChannelFrame))
        return static_cast<void*>(const_cast< FindChannelFrame*>(this));
    if (!strcmp(_clname, "FindChannelFrameClass"))
        return static_cast< FindChannelFrameClass*>(const_cast< FindChannelFrame*>(this));
    return ServerFrame::qt_metacast(_clname);
}

int MoodBox::FindChannelFrame::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerFrame::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onSearchContacts((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< ChannelSearchResult(*)>(_a[2]))); break;
        case 1: onRequestCancelled(); break;
        case 2: onContactListChanged(); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
