/****************************************************************************
** Meta object code from reading C++ file 'colorwidget.h'
**
** Created: Tue May 5 10:34:38 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "colorwidget.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'colorwidget.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__ColorWidget[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      34,   22,   21,   21, 0x05,
      66,   22,   21,   21, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ColorWidget[] = {
    "MoodBox::ColorWidget\0\0colorWidget\0"
    "activationRequest(ColorWidget*)\0"
    "editRequest(ColorWidget*)\0"
};

const QMetaObject MoodBox::ColorWidget::staticMetaObject = {
    { &ColorControl::staticMetaObject, qt_meta_stringdata_MoodBox__ColorWidget,
      qt_meta_data_MoodBox__ColorWidget, 0 }
};

const QMetaObject *MoodBox::ColorWidget::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ColorWidget::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ColorWidget))
        return static_cast<void*>(const_cast< ColorWidget*>(this));
    return ColorControl::qt_metacast(_clname);
}

int MoodBox::ColorWidget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ColorControl::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: activationRequest((*reinterpret_cast< ColorWidget*(*)>(_a[1]))); break;
        case 1: editRequest((*reinterpret_cast< ColorWidget*(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::ColorWidget::activationRequest(ColorWidget * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::ColorWidget::editRequest(ColorWidget * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
QT_END_MOC_NAMESPACE
