/****************************************************************************
** Meta object code from reading C++ file 'eyedroppertool.h'
**
** Created: Wed Jun 24 21:01:26 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../Velasquez/Qt/eyedroppertool.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'eyedroppertool.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Velasquez__EyedropperTool[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      33,   27,   26,   26, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_Velasquez__EyedropperTool[] = {
    "Velasquez::EyedropperTool\0\0color\0"
    "colorPicked(QColor)\0"
};

const QMetaObject Velasquez::EyedropperTool::staticMetaObject = {
    { &DrawingTool::staticMetaObject, qt_meta_stringdata_Velasquez__EyedropperTool,
      qt_meta_data_Velasquez__EyedropperTool, 0 }
};

const QMetaObject *Velasquez::EyedropperTool::metaObject() const
{
    return &staticMetaObject;
}

void *Velasquez::EyedropperTool::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Velasquez__EyedropperTool))
        return static_cast<void*>(const_cast< EyedropperTool*>(this));
    return DrawingTool::qt_metacast(_clname);
}

int Velasquez::EyedropperTool::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = DrawingTool::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: colorPicked((*reinterpret_cast< const QColor(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}

// SIGNAL 0
void Velasquez::EyedropperTool::colorPicked(const QColor & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
