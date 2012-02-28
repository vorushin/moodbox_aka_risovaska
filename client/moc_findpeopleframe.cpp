/****************************************************************************
** Meta object code from reading C++ file 'findpeopleframe.h'
**
** Created: Wed Jun 24 21:01:11 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "findpeopleframe.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'findpeopleframe.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__SearchPeopleRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      43,   30,   29,   29, 0x05,

 // slots: signature, parameters, type, tag, flags
     102,   83,   29,   29, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__SearchPeopleRequest[] = {
    "MoodBox::SearchPeopleRequest\0\0"
    "fault,result\0searchCompleted(Fault,UserSearchResult)\0"
    "state,fault,result\0"
    "onSearchResult(QVariant,Fault,UserSearchResult)\0"
};

const QMetaObject MoodBox::SearchPeopleRequest::staticMetaObject = {
    { &ServerRequest::staticMetaObject, qt_meta_stringdata_MoodBox__SearchPeopleRequest,
      qt_meta_data_MoodBox__SearchPeopleRequest, 0 }
};

const QMetaObject *MoodBox::SearchPeopleRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::SearchPeopleRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__SearchPeopleRequest))
        return static_cast<void*>(const_cast< SearchPeopleRequest*>(this));
    return ServerRequest::qt_metacast(_clname);
}

int MoodBox::SearchPeopleRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: searchCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< UserSearchResult(*)>(_a[2]))); break;
        case 1: onSearchResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< UserSearchResult(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::SearchPeopleRequest::searchCompleted(Fault _t1, UserSearchResult _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__FindPeopleFrame[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      13,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      30,   26,   25,   25, 0x05,

 // slots: signature, parameters, type, tag, flags
      49,   25,   25,   25, 0x0a,
      76,   63,   25,   25, 0x0a,
     117,   25,   25,   25, 0x0a,
     138,   25,   25,   25, 0x08,
     174,  168,   25,   25, 0x08,
     270,  218,   25,   25, 0x08,
     319,  308,   25,   25, 0x08,
     342,  308,   25,   25, 0x08,
     371,  308,   25,   25, 0x08,
     398,  394,   25,   25, 0x08,
     423,  394,   25,   25, 0x08,
     450,   25,   25,   25, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__FindPeopleFrame[] = {
    "MoodBox::FindPeopleFrame\0\0can\0"
    "canAddFriend(bool)\0addAsFriend()\0"
    "fault,result\0onSearchContacts(Fault,UserSearchResult)\0"
    "onRequestCancelled()\0on_findPeopleButton_clicked()\0"
    "state\0on_advancedSearchCheckBox_stateChanged(int)\0"
    "currentRow,currentColumn,previousRow,previousColumn\0"
    "onCurrentCellChanged(int,int,int,int)\0"
    "row,column\0onCellClicked(int,int)\0"
    "onCellDoubleClicked(int,int)\0"
    "onCellEntered(int,int)\0row\0"
    "onUserInfoTriggered(int)\0"
    "onWebProfileTriggered(int)\0"
    "showFoundContacts()\0"
};

const QMetaObject MoodBox::FindPeopleFrame::staticMetaObject = {
    { &ServerFrame::staticMetaObject, qt_meta_stringdata_MoodBox__FindPeopleFrame,
      qt_meta_data_MoodBox__FindPeopleFrame, 0 }
};

const QMetaObject *MoodBox::FindPeopleFrame::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::FindPeopleFrame::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__FindPeopleFrame))
        return static_cast<void*>(const_cast< FindPeopleFrame*>(this));
    if (!strcmp(_clname, "FindPeopleFrameClass"))
        return static_cast< FindPeopleFrameClass*>(const_cast< FindPeopleFrame*>(this));
    return ServerFrame::qt_metacast(_clname);
}

int MoodBox::FindPeopleFrame::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerFrame::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: canAddFriend((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 1: addAsFriend(); break;
        case 2: onSearchContacts((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< UserSearchResult(*)>(_a[2]))); break;
        case 3: onRequestCancelled(); break;
        case 4: on_findPeopleButton_clicked(); break;
        case 5: on_advancedSearchCheckBox_stateChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 6: onCurrentCellChanged((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2])),(*reinterpret_cast< int(*)>(_a[3])),(*reinterpret_cast< int(*)>(_a[4]))); break;
        case 7: onCellClicked((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 8: onCellDoubleClicked((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 9: onCellEntered((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 10: onUserInfoTriggered((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 11: onWebProfileTriggered((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 12: showFoundContacts(); break;
        default: ;
        }
        _id -= 13;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::FindPeopleFrame::canAddFriend(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
