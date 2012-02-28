/****************************************************************************
** Meta object code from reading C++ file 'formblocker.h'
**
** Created: Tue May 5 10:34:42 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "formblocker.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'formblocker.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__FormBlocker[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      22,   21,   21,   21, 0x05,

 // slots: signature, parameters, type, tag, flags
      42,   21,   21,   21, 0x09,
      61,   21,   21,   21, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__FormBlocker[] = {
    "MoodBox::FormBlocker\0\0progressCancelled()\0"
    "onCancelProgress()\0onTimeoutShowProgressBar()\0"
};

const QMetaObject MoodBox::FormBlocker::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MoodBox__FormBlocker,
      qt_meta_data_MoodBox__FormBlocker, 0 }
};

const QMetaObject *MoodBox::FormBlocker::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::FormBlocker::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__FormBlocker))
        return static_cast<void*>(const_cast< FormBlocker*>(this));
    return QObject::qt_metacast(_clname);
}

int MoodBox::FormBlocker::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: progressCancelled(); break;
        case 1: onCancelProgress(); break;
        case 2: onTimeoutShowProgressBar(); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::FormBlocker::progressCancelled()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}
QT_END_MOC_NAMESPACE
