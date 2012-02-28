/****************************************************************************
** Meta object code from reading C++ file 'timerobjects.h'
**
** Created: Tue May 5 10:34:55 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "timerobjects.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'timerobjects.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__SingleShotTimerObject[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      32,   31,   31,   31, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__SingleShotTimerObject[] = {
    "MoodBox::SingleShotTimerObject\0\0"
    "onTimerTimeout()\0"
};

const QMetaObject MoodBox::SingleShotTimerObject::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MoodBox__SingleShotTimerObject,
      qt_meta_data_MoodBox__SingleShotTimerObject, 0 }
};

const QMetaObject *MoodBox::SingleShotTimerObject::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::SingleShotTimerObject::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__SingleShotTimerObject))
        return static_cast<void*>(const_cast< SingleShotTimerObject*>(this));
    return QObject::qt_metacast(_clname);
}

int MoodBox::SingleShotTimerObject::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onTimerTimeout(); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
static const uint qt_meta_data_MoodBox__RetryTimerObject[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       0,    0, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__RetryTimerObject[] = {
    "MoodBox::RetryTimerObject\0"
};

const QMetaObject MoodBox::RetryTimerObject::staticMetaObject = {
    { &SingleShotTimerObject::staticMetaObject, qt_meta_stringdata_MoodBox__RetryTimerObject,
      qt_meta_data_MoodBox__RetryTimerObject, 0 }
};

const QMetaObject *MoodBox::RetryTimerObject::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::RetryTimerObject::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__RetryTimerObject))
        return static_cast<void*>(const_cast< RetryTimerObject*>(this));
    return SingleShotTimerObject::qt_metacast(_clname);
}

int MoodBox::RetryTimerObject::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = SingleShotTimerObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
static const uint qt_meta_data_MoodBox__RandomRetryTimerObject[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       0,    0, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__RandomRetryTimerObject[] = {
    "MoodBox::RandomRetryTimerObject\0"
};

const QMetaObject MoodBox::RandomRetryTimerObject::staticMetaObject = {
    { &RetryTimerObject::staticMetaObject, qt_meta_stringdata_MoodBox__RandomRetryTimerObject,
      qt_meta_data_MoodBox__RandomRetryTimerObject, 0 }
};

const QMetaObject *MoodBox::RandomRetryTimerObject::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::RandomRetryTimerObject::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__RandomRetryTimerObject))
        return static_cast<void*>(const_cast< RandomRetryTimerObject*>(this));
    return RetryTimerObject::qt_metacast(_clname);
}

int MoodBox::RandomRetryTimerObject::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = RetryTimerObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
QT_END_MOC_NAMESPACE
