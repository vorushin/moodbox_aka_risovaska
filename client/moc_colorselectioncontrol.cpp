/****************************************************************************
** Meta object code from reading C++ file 'colorselectioncontrol.h'
**
** Created: Tue May 5 10:34:38 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "colorselectioncontrol.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'colorselectioncontrol.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__ColorSelectionControl[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      38,   32,   31,   31, 0x05,

 // slots: signature, parameters, type, tag, flags
      60,   32,   31,   31, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ColorSelectionControl[] = {
    "MoodBox::ColorSelectionControl\0\0color\0"
    "colorSelected(QColor)\0setColor(QColor)\0"
};

const QMetaObject MoodBox::ColorSelectionControl::staticMetaObject = {
    { &ColorControl::staticMetaObject, qt_meta_stringdata_MoodBox__ColorSelectionControl,
      qt_meta_data_MoodBox__ColorSelectionControl, 0 }
};

const QMetaObject *MoodBox::ColorSelectionControl::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ColorSelectionControl::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ColorSelectionControl))
        return static_cast<void*>(const_cast< ColorSelectionControl*>(this));
    return ColorControl::qt_metacast(_clname);
}

int MoodBox::ColorSelectionControl::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ColorControl::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: colorSelected((*reinterpret_cast< const QColor(*)>(_a[1]))); break;
        case 1: setColor((*reinterpret_cast< const QColor(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::ColorSelectionControl::colorSelected(const QColor & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
