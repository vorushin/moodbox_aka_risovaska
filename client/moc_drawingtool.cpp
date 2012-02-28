/****************************************************************************
** Meta object code from reading C++ file 'drawingtool.h'
**
** Created: Wed Jun 24 21:01:26 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../Velasquez/Qt/drawingtool.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'drawingtool.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Velasquez__DrawingTool[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      32,   24,   23,   23, 0x05,
      89,   65,   23,   23, 0x05,

 // slots: signature, parameters, type, tag, flags
     139,   23,   23,   23, 0x09,
     160,  157,   23,   23, 0x09,
     185,   24,   23,   23, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_Velasquez__DrawingTool[] = {
    "Velasquez::DrawingTool\0\0element\0"
    "elementSelected(DrawingElement*)\0"
    "oldSelected,newSelected\0"
    "selectionChanged(DrawingElement*,DrawingElement*)\0"
    "updateSelection()\0id\0onSettingChanged(qint32)\0"
    "onRemoveElement(DrawingElement*)\0"
};

const QMetaObject Velasquez::DrawingTool::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_Velasquez__DrawingTool,
      qt_meta_data_Velasquez__DrawingTool, 0 }
};

const QMetaObject *Velasquez::DrawingTool::metaObject() const
{
    return &staticMetaObject;
}

void *Velasquez::DrawingTool::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Velasquez__DrawingTool))
        return static_cast<void*>(const_cast< DrawingTool*>(this));
    return QObject::qt_metacast(_clname);
}

int Velasquez::DrawingTool::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: elementSelected((*reinterpret_cast< DrawingElement*(*)>(_a[1]))); break;
        case 1: selectionChanged((*reinterpret_cast< DrawingElement*(*)>(_a[1])),(*reinterpret_cast< DrawingElement*(*)>(_a[2]))); break;
        case 2: updateSelection(); break;
        case 3: onSettingChanged((*reinterpret_cast< qint32(*)>(_a[1]))); break;
        case 4: onRemoveElement((*reinterpret_cast< DrawingElement*(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 5;
    }
    return _id;
}

// SIGNAL 0
void Velasquez::DrawingTool::elementSelected(DrawingElement * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void Velasquez::DrawingTool::selectionChanged(DrawingElement * _t1, DrawingElement * _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
QT_END_MOC_NAMESPACE
