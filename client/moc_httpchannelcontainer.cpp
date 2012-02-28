/****************************************************************************
** Meta object code from reading C++ file 'httpchannelcontainer.h'
**
** Created: Wed Jun 24 21:01:12 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "httpchannelcontainer.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'httpchannelcontainer.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__HttpChannelContainer[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      31,   30,   30,   30, 0x0a,
      48,   30,   30,   30, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__HttpChannelContainer[] = {
    "MoodBox::HttpChannelContainer\0\0"
    "handleResponse()\0handleFault()\0"
};

const QMetaObject MoodBox::HttpChannelContainer::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MoodBox__HttpChannelContainer,
      qt_meta_data_MoodBox__HttpChannelContainer, 0 }
};

const QMetaObject *MoodBox::HttpChannelContainer::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::HttpChannelContainer::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__HttpChannelContainer))
        return static_cast<void*>(const_cast< HttpChannelContainer*>(this));
    return QObject::qt_metacast(_clname);
}

int MoodBox::HttpChannelContainer::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: handleResponse(); break;
        case 1: handleFault(); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
