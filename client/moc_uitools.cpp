/****************************************************************************
** Meta object code from reading C++ file 'uitools.h'
**
** Created: Wed Jun 24 21:01:20 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "uitools.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'uitools.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__MoodBoxDialog[] = {

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

static const char qt_meta_stringdata_MoodBox__MoodBoxDialog[] = {
    "MoodBox::MoodBoxDialog\0"
};

const QMetaObject MoodBox::MoodBoxDialog::staticMetaObject = {
    { &QDialog::staticMetaObject, qt_meta_stringdata_MoodBox__MoodBoxDialog,
      qt_meta_data_MoodBox__MoodBoxDialog, 0 }
};

const QMetaObject *MoodBox::MoodBoxDialog::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::MoodBoxDialog::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__MoodBoxDialog))
        return static_cast<void*>(const_cast< MoodBoxDialog*>(this));
    return QDialog::qt_metacast(_clname);
}

int MoodBox::MoodBoxDialog::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
static const uint qt_meta_data_MoodBox__MovableWidget[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      31,   24,   23,   23, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__MovableWidget[] = {
    "MoodBox::MovableWidget\0\0newPos\0"
    "dragged(QPoint)\0"
};

const QMetaObject MoodBox::MovableWidget::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_MoodBox__MovableWidget,
      qt_meta_data_MoodBox__MovableWidget, 0 }
};

const QMetaObject *MoodBox::MovableWidget::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::MovableWidget::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__MovableWidget))
        return static_cast<void*>(const_cast< MovableWidget*>(this));
    return QWidget::qt_metacast(_clname);
}

int MoodBox::MovableWidget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: dragged((*reinterpret_cast< const QPoint(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::MovableWidget::dragged(const QPoint & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__ContactTable[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      23,   22,   22,   22, 0x05,
      46,   42,   22,   22, 0x05,
      69,   42,   22,   22, 0x05,

 // slots: signature, parameters, type, tag, flags
      94,   22,   22,   22, 0x08,
     116,   22,   22,   22, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ContactTable[] = {
    "MoodBox::ContactTable\0\0addUserTriggered()\0"
    "row\0userInfoTriggered(int)\0"
    "webProfileTriggered(int)\0onUserInfoTriggered()\0"
    "onWebProfileTriggered()\0"
};

const QMetaObject MoodBox::ContactTable::staticMetaObject = {
    { &QTableWidget::staticMetaObject, qt_meta_stringdata_MoodBox__ContactTable,
      qt_meta_data_MoodBox__ContactTable, 0 }
};

const QMetaObject *MoodBox::ContactTable::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ContactTable::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ContactTable))
        return static_cast<void*>(const_cast< ContactTable*>(this));
    return QTableWidget::qt_metacast(_clname);
}

int MoodBox::ContactTable::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QTableWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: addUserTriggered(); break;
        case 1: userInfoTriggered((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 2: webProfileTriggered((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 3: onUserInfoTriggered(); break;
        case 4: onWebProfileTriggered(); break;
        default: ;
        }
        _id -= 5;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::ContactTable::addUserTriggered()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void MoodBox::ContactTable::userInfoTriggered(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void MoodBox::ContactTable::webProfileTriggered(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}
QT_END_MOC_NAMESPACE
