/****************************************************************************
** Meta object code from reading C++ file 'transformableelement.h'
**
** Created: Tue May 5 10:35:04 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../Velasquez/Qt/transformableelement.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'transformableelement.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Velasquez__TransformableElement[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      24,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      41,   33,   32,   32, 0x05,
      64,   56,   32,   32, 0x05,
      98,   56,   32,   32, 0x05,
     131,   56,   32,   32, 0x05,
     180,  165,   32,   32, 0x05,
     217,   56,   32,   32, 0x05,
     269,  241,   32,   32, 0x05,
     317,   56,   32,   32, 0x05,
     348,   56,   32,   32, 0x05,
     382,   56,   32,   32, 0x05,
     417,   56,   32,   32, 0x05,
     453,   56,   32,   32, 0x05,

 // slots: signature, parameters, type, tag, flags
     489,  485,   32,   32, 0x09,
     505,   32,   32,   32, 0x09,
     517,   32,   32,   32, 0x09,
     528,   32,   32,   32, 0x09,
     555,   32,   32,   32, 0x09,
     604,  584,   32,   32, 0x09,
     632,   32,   32,   32, 0x09,
     647,   32,   32,   32, 0x09,
     663,   32,   32,   32, 0x09,
     680,   32,   32,   32, 0x09,
     693,   32,   32,   32, 0x09,
     713,   32,   32,   32, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_Velasquez__TransformableElement[] = {
    "Velasquez::TransformableElement\0\0"
    "started\0changing(bool)\0element\0"
    "mouseEnter(TransformableElement*)\0"
    "mouseMove(TransformableElement*)\0"
    "mouseLeave(TransformableElement*)\0"
    "element,oldPos\0moved(TransformableElement*,QPointF)\0"
    "remove(DrawingElement*)\0"
    "element,diffScale,diffAngle\0"
    "resizeRotate(TransformableElement*,qreal,qreal)\0"
    "reflect(TransformableElement*)\0"
    "bringFront(TransformableElement*)\0"
    "moveForward(TransformableElement*)\0"
    "moveBackward(TransformableElement*)\0"
    "sendBack(TransformableElement*)\0pos\0"
    "onMenu(QPointF)\0onReflect()\0onRemove()\0"
    "onResizeRotateHoverPress()\0"
    "onResizeRotateHoverRelease()\0"
    "diffScale,diffAngle\0onResizeRotate(qreal,qreal)\0"
    "onBringFront()\0onMoveForward()\0"
    "onMoveBackward()\0onSendBack()\0"
    "onContextMenuShow()\0onContextMenuHide()\0"
};

const QMetaObject Velasquez::TransformableElement::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_Velasquez__TransformableElement,
      qt_meta_data_Velasquez__TransformableElement, 0 }
};

const QMetaObject *Velasquez::TransformableElement::metaObject() const
{
    return &staticMetaObject;
}

void *Velasquez::TransformableElement::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Velasquez__TransformableElement))
        return static_cast<void*>(const_cast< TransformableElement*>(this));
    if (!strcmp(_clname, "DrawingElement"))
        return static_cast< DrawingElement*>(const_cast< TransformableElement*>(this));
    return QObject::qt_metacast(_clname);
}

int Velasquez::TransformableElement::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: changing((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 1: mouseEnter((*reinterpret_cast< TransformableElement*(*)>(_a[1]))); break;
        case 2: mouseMove((*reinterpret_cast< TransformableElement*(*)>(_a[1]))); break;
        case 3: mouseLeave((*reinterpret_cast< TransformableElement*(*)>(_a[1]))); break;
        case 4: moved((*reinterpret_cast< TransformableElement*(*)>(_a[1])),(*reinterpret_cast< const QPointF(*)>(_a[2]))); break;
        case 5: remove((*reinterpret_cast< DrawingElement*(*)>(_a[1]))); break;
        case 6: resizeRotate((*reinterpret_cast< TransformableElement*(*)>(_a[1])),(*reinterpret_cast< qreal(*)>(_a[2])),(*reinterpret_cast< qreal(*)>(_a[3]))); break;
        case 7: reflect((*reinterpret_cast< TransformableElement*(*)>(_a[1]))); break;
        case 8: bringFront((*reinterpret_cast< TransformableElement*(*)>(_a[1]))); break;
        case 9: moveForward((*reinterpret_cast< TransformableElement*(*)>(_a[1]))); break;
        case 10: moveBackward((*reinterpret_cast< TransformableElement*(*)>(_a[1]))); break;
        case 11: sendBack((*reinterpret_cast< TransformableElement*(*)>(_a[1]))); break;
        case 12: onMenu((*reinterpret_cast< const QPointF(*)>(_a[1]))); break;
        case 13: onReflect(); break;
        case 14: onRemove(); break;
        case 15: onResizeRotateHoverPress(); break;
        case 16: onResizeRotateHoverRelease(); break;
        case 17: onResizeRotate((*reinterpret_cast< qreal(*)>(_a[1])),(*reinterpret_cast< qreal(*)>(_a[2]))); break;
        case 18: onBringFront(); break;
        case 19: onMoveForward(); break;
        case 20: onMoveBackward(); break;
        case 21: onSendBack(); break;
        case 22: onContextMenuShow(); break;
        case 23: onContextMenuHide(); break;
        default: ;
        }
        _id -= 24;
    }
    return _id;
}

// SIGNAL 0
void Velasquez::TransformableElement::changing(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void Velasquez::TransformableElement::mouseEnter(TransformableElement * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void Velasquez::TransformableElement::mouseMove(TransformableElement * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void Velasquez::TransformableElement::mouseLeave(TransformableElement * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}

// SIGNAL 4
void Velasquez::TransformableElement::moved(TransformableElement * _t1, const QPointF & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 4, _a);
}

// SIGNAL 5
void Velasquez::TransformableElement::remove(DrawingElement * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 5, _a);
}

// SIGNAL 6
void Velasquez::TransformableElement::resizeRotate(TransformableElement * _t1, qreal _t2, qreal _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 6, _a);
}

// SIGNAL 7
void Velasquez::TransformableElement::reflect(TransformableElement * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 7, _a);
}

// SIGNAL 8
void Velasquez::TransformableElement::bringFront(TransformableElement * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 8, _a);
}

// SIGNAL 9
void Velasquez::TransformableElement::moveForward(TransformableElement * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 9, _a);
}

// SIGNAL 10
void Velasquez::TransformableElement::moveBackward(TransformableElement * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 10, _a);
}

// SIGNAL 11
void Velasquez::TransformableElement::sendBack(TransformableElement * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 11, _a);
}
QT_END_MOC_NAMESPACE
