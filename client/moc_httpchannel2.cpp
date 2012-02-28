/****************************************************************************
** Meta object code from reading C++ file 'httpchannel2.h'
**
** Created: Wed Jun 24 21:01:12 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "httpchannel2.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'httpchannel2.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__HttpChannel2[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      23,   22,   22,   22, 0x08,
      50,   44,   22,   22, 0x08,
      82,   22,   22,   22, 0x08,
     113,  102,   22,   22, 0x08,
     150,   22,   22,   22, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__HttpChannel2[] = {
    "MoodBox::HttpChannel2\0\0sendPendingRequest()\0"
    "reply\0requestFinished(QNetworkReply*)\0"
    "handleAndContinue()\0done,total\0"
    "dataProcessedProgress(qint64,qint64)\0"
    "onAbort()\0"
};

const QMetaObject MoodBox::HttpChannel2::staticMetaObject = {
    { &TransportChannelBase::staticMetaObject, qt_meta_stringdata_MoodBox__HttpChannel2,
      qt_meta_data_MoodBox__HttpChannel2, 0 }
};

const QMetaObject *MoodBox::HttpChannel2::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::HttpChannel2::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__HttpChannel2))
        return static_cast<void*>(const_cast< HttpChannel2*>(this));
    return TransportChannelBase::qt_metacast(_clname);
}

int MoodBox::HttpChannel2::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = TransportChannelBase::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: sendPendingRequest(); break;
        case 1: requestFinished((*reinterpret_cast< QNetworkReply*(*)>(_a[1]))); break;
        case 2: handleAndContinue(); break;
        case 3: dataProcessedProgress((*reinterpret_cast< qint64(*)>(_a[1])),(*reinterpret_cast< qint64(*)>(_a[2]))); break;
        case 4: onAbort(); break;
        default: ;
        }
        _id -= 5;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
