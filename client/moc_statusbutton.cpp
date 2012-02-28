/****************************************************************************
** Meta object code from reading C++ file 'statusbutton.h'
**
** Created: Tue May 5 10:34:55 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "statusbutton.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'statusbutton.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__StatusButton[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      23,   22,   22,   22, 0x0a,
      43,   22,   22,   22, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__StatusButton[] = {
    "MoodBox::StatusButton\0\0onMenuAboutToShow()\0"
    "onMenuAboutToHide()\0"
};

const QMetaObject MoodBox::StatusButton::staticMetaObject = {
    { &QToolButton::staticMetaObject, qt_meta_stringdata_MoodBox__StatusButton,
      qt_meta_data_MoodBox__StatusButton, 0 }
};

const QMetaObject *MoodBox::StatusButton::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::StatusButton::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__StatusButton))
        return static_cast<void*>(const_cast< StatusButton*>(this));
    if (!strcmp(_clname, "StatusButtonClass"))
        return static_cast< StatusButtonClass*>(const_cast< StatusButton*>(this));
    return QToolButton::qt_metacast(_clname);
}

int MoodBox::StatusButton::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QToolButton::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onMenuAboutToShow(); break;
        case 1: onMenuAboutToHide(); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
