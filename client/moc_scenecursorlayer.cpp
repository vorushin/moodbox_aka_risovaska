/****************************************************************************
** Meta object code from reading C++ file 'scenecursorlayer.h'
**
** Created: Tue May 5 10:35:03 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../Velasquez/Qt/scenecursorlayer.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'scenecursorlayer.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Velasquez__SceneCursorLayer[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      34,   29,   28,   28, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_Velasquez__SceneCursorLayer[] = {
    "Velasquez::SceneCursorLayer\0\0rect\0"
    "onSceneRectChanged(QRectF)\0"
};

const QMetaObject Velasquez::SceneCursorLayer::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_Velasquez__SceneCursorLayer,
      qt_meta_data_Velasquez__SceneCursorLayer, 0 }
};

const QMetaObject *Velasquez::SceneCursorLayer::metaObject() const
{
    return &staticMetaObject;
}

void *Velasquez::SceneCursorLayer::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Velasquez__SceneCursorLayer))
        return static_cast<void*>(const_cast< SceneCursorLayer*>(this));
    if (!strcmp(_clname, "QGraphicsItem"))
        return static_cast< QGraphicsItem*>(const_cast< SceneCursorLayer*>(this));
    return QObject::qt_metacast(_clname);
}

int Velasquez::SceneCursorLayer::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onSceneRectChanged((*reinterpret_cast< const QRectF(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
