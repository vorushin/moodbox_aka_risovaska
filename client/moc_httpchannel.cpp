/****************************************************************************
** Meta object code from reading C++ file 'httpchannel.h'
**
** Created: Wed Jun 24 21:01:12 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "httpchannel.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'httpchannel.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__HttpChannel[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      40,   22,   21,   21, 0x08,
      77,   66,   21,   21, 0x08,
     108,   21,   21,   21, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__HttpChannel[] = {
    "MoodBox::HttpChannel\0\0requestId,isError\0"
    "requestFinished(int,bool)\0done,total\0"
    "dataProcessedProgress(int,int)\0onAbort()\0"
};

const QMetaObject MoodBox::HttpChannel::staticMetaObject = {
    { &TransportChannelBase::staticMetaObject, qt_meta_stringdata_MoodBox__HttpChannel,
      qt_meta_data_MoodBox__HttpChannel, 0 }
};

const QMetaObject *MoodBox::HttpChannel::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::HttpChannel::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__HttpChannel))
        return static_cast<void*>(const_cast< HttpChannel*>(this));
    return TransportChannelBase::qt_metacast(_clname);
}

int MoodBox::HttpChannel::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = TransportChannelBase::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: requestFinished((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< bool(*)>(_a[2]))); break;
        case 1: dataProcessedProgress((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 2: onAbort(); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
