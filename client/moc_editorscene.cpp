/****************************************************************************
** Meta object code from reading C++ file 'editorscene.h'
**
** Created: Tue May 5 10:35:02 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../Velasquez/Qt/editorscene.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'editorscene.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Velasquez__EditorScene[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      24,   23,   23,   23, 0x05,
      40,   34,   23,   23, 0x05,
      76,   71,   23,   23, 0x05,
     102,   71,   23,   23, 0x05,

 // slots: signature, parameters, type, tag, flags
     130,   23,   23,   23, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_Velasquez__EditorScene[] = {
    "Velasquez::EditorScene\0\0cleared()\0"
    "color\0backgroundColorChanged(QColor)\0"
    "item\0itemAdded(QGraphicsItem*)\0"
    "itemRemoved(QGraphicsItem*)\0"
    "onSelectionChanged()\0"
};

const QMetaObject Velasquez::EditorScene::staticMetaObject = {
    { &QGraphicsScene::staticMetaObject, qt_meta_stringdata_Velasquez__EditorScene,
      qt_meta_data_Velasquez__EditorScene, 0 }
};

const QMetaObject *Velasquez::EditorScene::metaObject() const
{
    return &staticMetaObject;
}

void *Velasquez::EditorScene::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Velasquez__EditorScene))
        return static_cast<void*>(const_cast< EditorScene*>(this));
    if (!strcmp(_clname, "MetaInfoProvider"))
        return static_cast< MetaInfoProvider*>(const_cast< EditorScene*>(this));
    return QGraphicsScene::qt_metacast(_clname);
}

int Velasquez::EditorScene::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QGraphicsScene::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: cleared(); break;
        case 1: backgroundColorChanged((*reinterpret_cast< const QColor(*)>(_a[1]))); break;
        case 2: itemAdded((*reinterpret_cast< QGraphicsItem*(*)>(_a[1]))); break;
        case 3: itemRemoved((*reinterpret_cast< QGraphicsItem*(*)>(_a[1]))); break;
        case 4: onSelectionChanged(); break;
        default: ;
        }
        _id -= 5;
    }
    return _id;
}

// SIGNAL 0
void Velasquez::EditorScene::cleared()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void Velasquez::EditorScene::backgroundColorChanged(const QColor & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void Velasquez::EditorScene::itemAdded(QGraphicsItem * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void Velasquez::EditorScene::itemRemoved(QGraphicsItem * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}
QT_END_MOC_NAMESPACE
