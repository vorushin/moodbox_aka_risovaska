/****************************************************************************
** Meta object code from reading C++ file 'blockedlistframe.h'
**
** Created: Wed Jun 24 21:01:07 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "blockedlistframe.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'blockedlistframe.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__BlockedListFrame[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      27,   26,   26,   26, 0x08,
      54,   26,   26,   26, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__BlockedListFrame[] = {
    "MoodBox::BlockedListFrame\0\0"
    "on_unblockButton_clicked()\0"
    "on_blockButton_clicked()\0"
};

const QMetaObject MoodBox::BlockedListFrame::staticMetaObject = {
    { &SetupDialogFrame::staticMetaObject, qt_meta_stringdata_MoodBox__BlockedListFrame,
      qt_meta_data_MoodBox__BlockedListFrame, 0 }
};

const QMetaObject *MoodBox::BlockedListFrame::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::BlockedListFrame::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__BlockedListFrame))
        return static_cast<void*>(const_cast< BlockedListFrame*>(this));
    if (!strcmp(_clname, "BlockedListFrameClass"))
        return static_cast< BlockedListFrameClass*>(const_cast< BlockedListFrame*>(this));
    return SetupDialogFrame::qt_metacast(_clname);
}

int MoodBox::BlockedListFrame::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = SetupDialogFrame::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: on_unblockButton_clicked(); break;
        case 1: on_blockButton_clicked(); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
