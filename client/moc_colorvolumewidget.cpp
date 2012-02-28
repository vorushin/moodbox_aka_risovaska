/****************************************************************************
** Meta object code from reading C++ file 'colorvolumewidget.h'
**
** Created: Tue May 5 10:34:38 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "colorvolumewidget.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'colorvolumewidget.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__ColorVolumeWidget[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      34,   28,   27,   27, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ColorVolumeWidget[] = {
    "MoodBox::ColorVolumeWidget\0\0color\0"
    "setColor(QColor)\0"
};

const QMetaObject MoodBox::ColorVolumeWidget::staticMetaObject = {
    { &ColorSelectionControl::staticMetaObject, qt_meta_stringdata_MoodBox__ColorVolumeWidget,
      qt_meta_data_MoodBox__ColorVolumeWidget, 0 }
};

const QMetaObject *MoodBox::ColorVolumeWidget::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ColorVolumeWidget::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ColorVolumeWidget))
        return static_cast<void*>(const_cast< ColorVolumeWidget*>(this));
    return ColorSelectionControl::qt_metacast(_clname);
}

int MoodBox::ColorVolumeWidget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ColorSelectionControl::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: setColor((*reinterpret_cast< const QColor(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
