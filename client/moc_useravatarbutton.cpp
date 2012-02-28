/****************************************************************************
** Meta object code from reading C++ file 'useravatarbutton.h'
**
** Created: Tue May 5 10:34:56 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "useravatarbutton.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'useravatarbutton.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__UserAvatarButton[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      31,   27,   26,   26, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__UserAvatarButton[] = {
    "MoodBox::UserAvatarButton\0\0key\0"
    "onNewPictureLoaded(QString)\0"
};

const QMetaObject MoodBox::UserAvatarButton::staticMetaObject = {
    { &AvatarButton::staticMetaObject, qt_meta_stringdata_MoodBox__UserAvatarButton,
      qt_meta_data_MoodBox__UserAvatarButton, 0 }
};

const QMetaObject *MoodBox::UserAvatarButton::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::UserAvatarButton::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__UserAvatarButton))
        return static_cast<void*>(const_cast< UserAvatarButton*>(this));
    return AvatarButton::qt_metacast(_clname);
}

int MoodBox::UserAvatarButton::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = AvatarButton::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onNewPictureLoaded((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
