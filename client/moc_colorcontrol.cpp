/****************************************************************************
** Meta object code from reading C++ file 'colorcontrol.h'
**
** Created: Tue May 5 10:34:38 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "colorcontrol.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'colorcontrol.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__ColorControl[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      29,   23,   22,   22, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ColorControl[] = {
    "MoodBox::ColorControl\0\0color\0"
    "setColor(QColor)\0"
};

const QMetaObject MoodBox::ColorControl::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_MoodBox__ColorControl,
      qt_meta_data_MoodBox__ColorControl, 0 }
};

const QMetaObject *MoodBox::ColorControl::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ColorControl::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ColorControl))
        return static_cast<void*>(const_cast< ColorControl*>(this));
    return QWidget::qt_metacast(_clname);
}

int MoodBox::ColorControl::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
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
