/****************************************************************************
** Meta object code from reading C++ file 'finddialog.h'
**
** Created: Wed Jun 24 21:01:10 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "finddialog.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'finddialog.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__FindDialog[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      21,   20,   20,   20, 0x08,
      57,   48,   20,   20, 0x08,
      94,   20,   20,   20, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__FindDialog[] = {
    "MoodBox::FindDialog\0\0on_addUserButton_clicked()\0"
    "tabIndex\0on_findTabWidget_currentChanged(int)\0"
    "onClose()\0"
};

const QMetaObject MoodBox::FindDialog::staticMetaObject = {
    { &ServerDialog::staticMetaObject, qt_meta_stringdata_MoodBox__FindDialog,
      qt_meta_data_MoodBox__FindDialog, 0 }
};

const QMetaObject *MoodBox::FindDialog::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::FindDialog::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__FindDialog))
        return static_cast<void*>(const_cast< FindDialog*>(this));
    if (!strcmp(_clname, "FindDialogClass"))
        return static_cast< FindDialogClass*>(const_cast< FindDialog*>(this));
    return ServerDialog::qt_metacast(_clname);
}

int MoodBox::FindDialog::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: on_addUserButton_clicked(); break;
        case 1: on_findTabWidget_currentChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 2: onClose(); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
