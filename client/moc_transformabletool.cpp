/****************************************************************************
** Meta object code from reading C++ file 'transformabletool.h'
**
** Created: Wed Jun 24 21:01:26 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../Velasquez/Qt/transformabletool.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'transformabletool.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Velasquez__TransformableTool[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      11,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      38,   30,   29,   29, 0x09,
      70,   62,   29,   29, 0x09,
     113,   62,   29,   29, 0x09,
     155,   62,   29,   29, 0x09,
     213,  198,   29,   29, 0x09,
     287,  259,   29,   29, 0x09,
     344,   62,   29,   29, 0x09,
     384,   62,   29,   29, 0x09,
     427,   62,   29,   29, 0x09,
     472,   62,   29,   29, 0x09,
     516,   62,   29,   29, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_Velasquez__TransformableTool[] = {
    "Velasquez::TransformableTool\0\0started\0"
    "onChangingSession(bool)\0element\0"
    "onElementMouseEnter(TransformableElement*)\0"
    "onElementMouseMove(TransformableElement*)\0"
    "onElementMouseLeave(TransformableElement*)\0"
    "element,oldPos\0"
    "onElementMoved(TransformableElement*,QPointF)\0"
    "element,diffScale,diffAngle\0"
    "onResizeRotateElement(TransformableElement*,qreal,qreal)\0"
    "onReflectElement(TransformableElement*)\0"
    "onBringFrontElement(TransformableElement*)\0"
    "onMoveBackwardElement(TransformableElement*)\0"
    "onMoveForwardElement(TransformableElement*)\0"
    "onSendBackElement(TransformableElement*)\0"
};

const QMetaObject Velasquez::TransformableTool::staticMetaObject = {
    { &DrawingTool::staticMetaObject, qt_meta_stringdata_Velasquez__TransformableTool,
      qt_meta_data_Velasquez__TransformableTool, 0 }
};

const QMetaObject *Velasquez::TransformableTool::metaObject() const
{
    return &staticMetaObject;
}

void *Velasquez::TransformableTool::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Velasquez__TransformableTool))
        return static_cast<void*>(const_cast< TransformableTool*>(this));
    return DrawingTool::qt_metacast(_clname);
}

int Velasquez::TransformableTool::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = DrawingTool::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onChangingSession((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 1: onElementMouseEnter((*reinterpret_cast< TransformableElement*(*)>(_a[1]))); break;
        case 2: onElementMouseMove((*reinterpret_cast< TransformableElement*(*)>(_a[1]))); break;
        case 3: onElementMouseLeave((*reinterpret_cast< TransformableElement*(*)>(_a[1]))); break;
        case 4: onElementMoved((*reinterpret_cast< TransformableElement*(*)>(_a[1])),(*reinterpret_cast< const QPointF(*)>(_a[2]))); break;
        case 5: onResizeRotateElement((*reinterpret_cast< TransformableElement*(*)>(_a[1])),(*reinterpret_cast< qreal(*)>(_a[2])),(*reinterpret_cast< qreal(*)>(_a[3]))); break;
        case 6: onReflectElement((*reinterpret_cast< TransformableElement*(*)>(_a[1]))); break;
        case 7: onBringFrontElement((*reinterpret_cast< TransformableElement*(*)>(_a[1]))); break;
        case 8: onMoveBackwardElement((*reinterpret_cast< TransformableElement*(*)>(_a[1]))); break;
        case 9: onMoveForwardElement((*reinterpret_cast< TransformableElement*(*)>(_a[1]))); break;
        case 10: onSendBackElement((*reinterpret_cast< TransformableElement*(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 11;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
