/****************************************************************************
** Meta object code from reading C++ file 'selectavatardialog.h'
**
** Created: Wed Jun 24 21:01:18 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "selectavatardialog.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'selectavatardialog.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__SelectAvatarDialog[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       4,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      29,   28,   28,   28, 0x08,
      58,   28,   28,   28, 0x08,
      80,   28,   28,   28, 0x08,
     113,   96,   28,   28, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__SelectAvatarDialog[] = {
    "MoodBox::SelectAvatarDialog\0\0"
    "on_addAvatarButton_clicked()\0"
    "on_okButton_clicked()\0onApplyAvatar()\0"
    "current,previous\0"
    "onItemChanged(QListWidgetItem*,QListWidgetItem*)\0"
};

const QMetaObject MoodBox::SelectAvatarDialog::staticMetaObject = {
    { &MoodBoxDialog::staticMetaObject, qt_meta_stringdata_MoodBox__SelectAvatarDialog,
      qt_meta_data_MoodBox__SelectAvatarDialog, 0 }
};

const QMetaObject *MoodBox::SelectAvatarDialog::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::SelectAvatarDialog::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__SelectAvatarDialog))
        return static_cast<void*>(const_cast< SelectAvatarDialog*>(this));
    if (!strcmp(_clname, "SelectAvatarDialogClass"))
        return static_cast< SelectAvatarDialogClass*>(const_cast< SelectAvatarDialog*>(this));
    return MoodBoxDialog::qt_metacast(_clname);
}

int MoodBox::SelectAvatarDialog::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = MoodBoxDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: on_addAvatarButton_clicked(); break;
        case 1: on_okButton_clicked(); break;
        case 2: onApplyAvatar(); break;
        case 3: onItemChanged((*reinterpret_cast< QListWidgetItem*(*)>(_a[1])),(*reinterpret_cast< QListWidgetItem*(*)>(_a[2]))); break;
        default: ;
        }
        _id -= 4;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
