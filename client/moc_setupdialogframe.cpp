/****************************************************************************
** Meta object code from reading C++ file 'setupdialogframe.h'
**
** Created: Wed Jun 24 21:01:20 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "setupdialogframe.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'setupdialogframe.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__SetupDialogFrame[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      27,   26,   26,   26, 0x05,
      44,   26,   26,   26, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__SetupDialogFrame[] = {
    "MoodBox::SetupDialogFrame\0\0updateFinished()\0"
    "updateError()\0"
};

const QMetaObject MoodBox::SetupDialogFrame::staticMetaObject = {
    { &ServerFrame::staticMetaObject, qt_meta_stringdata_MoodBox__SetupDialogFrame,
      qt_meta_data_MoodBox__SetupDialogFrame, 0 }
};

const QMetaObject *MoodBox::SetupDialogFrame::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::SetupDialogFrame::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__SetupDialogFrame))
        return static_cast<void*>(const_cast< SetupDialogFrame*>(this));
    return ServerFrame::qt_metacast(_clname);
}

int MoodBox::SetupDialogFrame::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerFrame::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: updateFinished(); break;
        case 1: updateError(); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::SetupDialogFrame::updateFinished()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void MoodBox::SetupDialogFrame::updateError()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}
QT_END_MOC_NAMESPACE
