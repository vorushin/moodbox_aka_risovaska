/****************************************************************************
** Meta object code from reading C++ file 'hoverpoints.h'
**
** Created: Tue May 5 10:35:02 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../Velasquez/Qt/hoverpoints.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'hoverpoints.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Velasquez__HoverPoint[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      27,   23,   22,   22, 0x05,
      44,   22,   22,   22, 0x25,
      54,   23,   22,   22, 0x05,
      72,   22,   22,   22, 0x25,

 // slots: signature, parameters, type, tag, flags
      83,   22,   22,   22, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_Velasquez__HoverPoint[] = {
    "Velasquez::HoverPoint\0\0pos\0pressed(QPointF)\0"
    "pressed()\0released(QPointF)\0released()\0"
    "updateAnchor()\0"
};

const QMetaObject Velasquez::HoverPoint::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_Velasquez__HoverPoint,
      qt_meta_data_Velasquez__HoverPoint, 0 }
};

const QMetaObject *Velasquez::HoverPoint::metaObject() const
{
    return &staticMetaObject;
}

void *Velasquez::HoverPoint::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Velasquez__HoverPoint))
        return static_cast<void*>(const_cast< HoverPoint*>(this));
    if (!strcmp(_clname, "QGraphicsItem"))
        return static_cast< QGraphicsItem*>(const_cast< HoverPoint*>(this));
    return QObject::qt_metacast(_clname);
}

int Velasquez::HoverPoint::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: pressed((*reinterpret_cast< const QPointF(*)>(_a[1]))); break;
        case 1: pressed(); break;
        case 2: released((*reinterpret_cast< const QPointF(*)>(_a[1]))); break;
        case 3: released(); break;
        case 4: updateAnchor(); break;
        default: ;
        }
        _id -= 5;
    }
    return _id;
}

// SIGNAL 0
void Velasquez::HoverPoint::pressed(const QPointF & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, 1, _a);
}

// SIGNAL 2
void Velasquez::HoverPoint::released(const QPointF & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, 3, _a);
}
static const uint qt_meta_data_Velasquez__RemoveHoverPoint[] = {

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

static const char qt_meta_stringdata_Velasquez__RemoveHoverPoint[] = {
    "Velasquez::RemoveHoverPoint\0"
};

const QMetaObject Velasquez::RemoveHoverPoint::staticMetaObject = {
    { &HoverPoint::staticMetaObject, qt_meta_stringdata_Velasquez__RemoveHoverPoint,
      qt_meta_data_Velasquez__RemoveHoverPoint, 0 }
};

const QMetaObject *Velasquez::RemoveHoverPoint::metaObject() const
{
    return &staticMetaObject;
}

void *Velasquez::RemoveHoverPoint::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Velasquez__RemoveHoverPoint))
        return static_cast<void*>(const_cast< RemoveHoverPoint*>(this));
    return HoverPoint::qt_metacast(_clname);
}

int Velasquez::RemoveHoverPoint::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = HoverPoint::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
static const uint qt_meta_data_Velasquez__ResizeRotateHoverPoint[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      55,   35,   34,   34, 0x05,

 // slots: signature, parameters, type, tag, flags
      81,   34,   34,   34, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_Velasquez__ResizeRotateHoverPoint[] = {
    "Velasquez::ResizeRotateHoverPoint\0\0"
    "diffScale,diffAngle\0resizeRotate(qreal,qreal)\0"
    "updateAnchor()\0"
};

const QMetaObject Velasquez::ResizeRotateHoverPoint::staticMetaObject = {
    { &HoverPoint::staticMetaObject, qt_meta_stringdata_Velasquez__ResizeRotateHoverPoint,
      qt_meta_data_Velasquez__ResizeRotateHoverPoint, 0 }
};

const QMetaObject *Velasquez::ResizeRotateHoverPoint::metaObject() const
{
    return &staticMetaObject;
}

void *Velasquez::ResizeRotateHoverPoint::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Velasquez__ResizeRotateHoverPoint))
        return static_cast<void*>(const_cast< ResizeRotateHoverPoint*>(this));
    return HoverPoint::qt_metacast(_clname);
}

int Velasquez::ResizeRotateHoverPoint::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = HoverPoint::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: resizeRotate((*reinterpret_cast< qreal(*)>(_a[1])),(*reinterpret_cast< qreal(*)>(_a[2]))); break;
        case 1: updateAnchor(); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void Velasquez::ResizeRotateHoverPoint::resizeRotate(qreal _t1, qreal _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_Velasquez__ReflectHoverPoint[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      43,   30,   29,   29, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_Velasquez__ReflectHoverPoint[] = {
    "Velasquez::ReflectHoverPoint\0\0"
    "newReflected\0reflected(bool)\0"
};

const QMetaObject Velasquez::ReflectHoverPoint::staticMetaObject = {
    { &HoverPoint::staticMetaObject, qt_meta_stringdata_Velasquez__ReflectHoverPoint,
      qt_meta_data_Velasquez__ReflectHoverPoint, 0 }
};

const QMetaObject *Velasquez::ReflectHoverPoint::metaObject() const
{
    return &staticMetaObject;
}

void *Velasquez::ReflectHoverPoint::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Velasquez__ReflectHoverPoint))
        return static_cast<void*>(const_cast< ReflectHoverPoint*>(this));
    return HoverPoint::qt_metacast(_clname);
}

int Velasquez::ReflectHoverPoint::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = HoverPoint::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: reflected((*reinterpret_cast< bool(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}

// SIGNAL 0
void Velasquez::ReflectHoverPoint::reflected(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_Velasquez__ResizeRotateCorner[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      31,   30,   30,   30, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_Velasquez__ResizeRotateCorner[] = {
    "Velasquez::ResizeRotateCorner\0\0"
    "updateAnchor()\0"
};

const QMetaObject Velasquez::ResizeRotateCorner::staticMetaObject = {
    { &ResizeRotateHoverPoint::staticMetaObject, qt_meta_stringdata_Velasquez__ResizeRotateCorner,
      qt_meta_data_Velasquez__ResizeRotateCorner, 0 }
};

const QMetaObject *Velasquez::ResizeRotateCorner::metaObject() const
{
    return &staticMetaObject;
}

void *Velasquez::ResizeRotateCorner::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Velasquez__ResizeRotateCorner))
        return static_cast<void*>(const_cast< ResizeRotateCorner*>(this));
    return ResizeRotateHoverPoint::qt_metacast(_clname);
}

int Velasquez::ResizeRotateCorner::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ResizeRotateHoverPoint::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: updateAnchor(); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
static const uint qt_meta_data_Velasquez__MenuHoverPoint[] = {

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

static const char qt_meta_stringdata_Velasquez__MenuHoverPoint[] = {
    "Velasquez::MenuHoverPoint\0"
};

const QMetaObject Velasquez::MenuHoverPoint::staticMetaObject = {
    { &HoverPoint::staticMetaObject, qt_meta_stringdata_Velasquez__MenuHoverPoint,
      qt_meta_data_Velasquez__MenuHoverPoint, 0 }
};

const QMetaObject *Velasquez::MenuHoverPoint::metaObject() const
{
    return &staticMetaObject;
}

void *Velasquez::MenuHoverPoint::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Velasquez__MenuHoverPoint))
        return static_cast<void*>(const_cast< MenuHoverPoint*>(this));
    return HoverPoint::qt_metacast(_clname);
}

int Velasquez::MenuHoverPoint::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = HoverPoint::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
QT_END_MOC_NAMESPACE
