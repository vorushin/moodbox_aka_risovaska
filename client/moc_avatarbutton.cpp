/****************************************************************************
** Meta object code from reading C++ file 'avatarbutton.h'
**
** Created: Tue May 5 10:34:34 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "avatarbutton.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'avatarbutton.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__AvatarButton[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      23,   22,   22,   22, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__AvatarButton[] = {
    "MoodBox::AvatarButton\0\0onPictureChanged()\0"
};

const QMetaObject MoodBox::AvatarButton::staticMetaObject = {
    { &QToolButton::staticMetaObject, qt_meta_stringdata_MoodBox__AvatarButton,
      qt_meta_data_MoodBox__AvatarButton, 0 }
};

const QMetaObject *MoodBox::AvatarButton::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::AvatarButton::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__AvatarButton))
        return static_cast<void*>(const_cast< AvatarButton*>(this));
    return QToolButton::qt_metacast(_clname);
}

int MoodBox::AvatarButton::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QToolButton::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onPictureChanged(); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
