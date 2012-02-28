/****************************************************************************
** Meta object code from reading C++ file 'mactools.h'
**
** Created: Wed Jun 24 21:01:13 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "mactools.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'mactools.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__AppEventHandler[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      26,   25,   25,   25, 0x05,
      46,   25,   25,   25, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__AppEventHandler[] = {
    "MoodBox::AppEventHandler\0\0reopenApplication()\0"
    "quitApplication()\0"
};

const QMetaObject MoodBox::AppEventHandler::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MoodBox__AppEventHandler,
      qt_meta_data_MoodBox__AppEventHandler, 0 }
};

const QMetaObject *MoodBox::AppEventHandler::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::AppEventHandler::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__AppEventHandler))
        return static_cast<void*>(const_cast< AppEventHandler*>(this));
    return QObject::qt_metacast(_clname);
}

int MoodBox::AppEventHandler::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: reopenApplication(); break;
        case 1: quitApplication(); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::AppEventHandler::reopenApplication()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void MoodBox::AppEventHandler::quitApplication()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}
static const uint qt_meta_data_MoodBox__DockIconHandler[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      35,   26,   25,   25, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__DockIconHandler[] = {
    "MoodBox::DockIconHandler\0\0newCount\0"
    "unreadCountChanged(int)\0"
};

const QMetaObject MoodBox::DockIconHandler::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MoodBox__DockIconHandler,
      qt_meta_data_MoodBox__DockIconHandler, 0 }
};

const QMetaObject *MoodBox::DockIconHandler::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::DockIconHandler::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__DockIconHandler))
        return static_cast<void*>(const_cast< DockIconHandler*>(this));
    return QObject::qt_metacast(_clname);
}

int MoodBox::DockIconHandler::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: unreadCountChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
