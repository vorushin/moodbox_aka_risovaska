/****************************************************************************
** Meta object code from reading C++ file 'soundsframe.h'
**
** Created: Tue May 5 10:34:54 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "soundsframe.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'soundsframe.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__SoundsFrame[] = {

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

static const char qt_meta_stringdata_MoodBox__SoundsFrame[] = {
    "MoodBox::SoundsFrame\0"
};

const QMetaObject MoodBox::SoundsFrame::staticMetaObject = {
    { &QFrame::staticMetaObject, qt_meta_stringdata_MoodBox__SoundsFrame,
      qt_meta_data_MoodBox__SoundsFrame, 0 }
};

const QMetaObject *MoodBox::SoundsFrame::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::SoundsFrame::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__SoundsFrame))
        return static_cast<void*>(const_cast< SoundsFrame*>(this));
    if (!strcmp(_clname, "SoundsFrameClass"))
        return static_cast< SoundsFrameClass*>(const_cast< SoundsFrame*>(this));
    return QFrame::qt_metacast(_clname);
}

int MoodBox::SoundsFrame::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QFrame::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
QT_END_MOC_NAMESPACE
