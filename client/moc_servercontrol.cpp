/****************************************************************************
** Meta object code from reading C++ file 'servercontrol.h'
**
** Created: Wed Jun 24 21:01:19 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "servercontrol.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'servercontrol.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__ServerDialog[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      23,   22,   22,   22, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ServerDialog[] = {
    "MoodBox::ServerDialog\0\0onRequestCancelled()\0"
};

const QMetaObject MoodBox::ServerDialog::staticMetaObject = {
    { &MoodBoxDialog::staticMetaObject, qt_meta_stringdata_MoodBox__ServerDialog,
      qt_meta_data_MoodBox__ServerDialog, 0 }
};

const QMetaObject *MoodBox::ServerDialog::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ServerDialog::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ServerDialog))
        return static_cast<void*>(const_cast< ServerDialog*>(this));
    if (!strcmp(_clname, "ServerControl"))
        return static_cast< ServerControl*>(const_cast< ServerDialog*>(this));
    return MoodBoxDialog::qt_metacast(_clname);
}

int MoodBox::ServerDialog::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = MoodBoxDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onRequestCancelled(); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
static const uint qt_meta_data_MoodBox__ServerWidget[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      23,   22,   22,   22, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ServerWidget[] = {
    "MoodBox::ServerWidget\0\0onRequestCancelled()\0"
};

const QMetaObject MoodBox::ServerWidget::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_MoodBox__ServerWidget,
      qt_meta_data_MoodBox__ServerWidget, 0 }
};

const QMetaObject *MoodBox::ServerWidget::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ServerWidget::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ServerWidget))
        return static_cast<void*>(const_cast< ServerWidget*>(this));
    if (!strcmp(_clname, "ServerControl"))
        return static_cast< ServerControl*>(const_cast< ServerWidget*>(this));
    return QWidget::qt_metacast(_clname);
}

int MoodBox::ServerWidget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onRequestCancelled(); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
static const uint qt_meta_data_MoodBox__ServerFrame[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      22,   21,   21,   21, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ServerFrame[] = {
    "MoodBox::ServerFrame\0\0onRequestCancelled()\0"
};

const QMetaObject MoodBox::ServerFrame::staticMetaObject = {
    { &QFrame::staticMetaObject, qt_meta_stringdata_MoodBox__ServerFrame,
      qt_meta_data_MoodBox__ServerFrame, 0 }
};

const QMetaObject *MoodBox::ServerFrame::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ServerFrame::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ServerFrame))
        return static_cast<void*>(const_cast< ServerFrame*>(this));
    if (!strcmp(_clname, "ServerControl"))
        return static_cast< ServerControl*>(const_cast< ServerFrame*>(this));
    return QFrame::qt_metacast(_clname);
}

int MoodBox::ServerFrame::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QFrame::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onRequestCancelled(); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
