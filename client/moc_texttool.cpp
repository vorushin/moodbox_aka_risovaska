/****************************************************************************
** Meta object code from reading C++ file 'texttool.h'
**
** Created: Wed Jun 24 21:01:26 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../Velasquez/Qt/texttool.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'texttool.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Velasquez__TextTool[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       8,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      29,   21,   20,   20, 0x09,
      60,   21,   20,   20, 0x09,
      92,   21,   20,   20, 0x09,
     120,   21,   20,   20, 0x09,
     150,   21,   20,   20, 0x08,
     186,   20,   20,   20, 0x08,
     221,  213,   20,   20, 0x08,
     252,  244,   20,   20, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_Velasquez__TextTool[] = {
    "Velasquez::TextTool\0\0element\0"
    "onEditingStarted(TextElement*)\0"
    "onEditingFinished(TextElement*)\0"
    "onTextChanged(TextElement*)\0"
    "onTextUndoAdded(TextElement*)\0"
    "onEditingElementDestroyed(QObject*)\0"
    "onCursorPointerDestroyed()\0canUndo\0"
    "onCanUndoChanged(bool)\0canRedo\0"
    "onCanRedoChanged(bool)\0"
};

const QMetaObject Velasquez::TextTool::staticMetaObject = {
    { &TransformableTool::staticMetaObject, qt_meta_stringdata_Velasquez__TextTool,
      qt_meta_data_Velasquez__TextTool, 0 }
};

const QMetaObject *Velasquez::TextTool::metaObject() const
{
    return &staticMetaObject;
}

void *Velasquez::TextTool::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Velasquez__TextTool))
        return static_cast<void*>(const_cast< TextTool*>(this));
    return TransformableTool::qt_metacast(_clname);
}

int Velasquez::TextTool::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = TransformableTool::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onEditingStarted((*reinterpret_cast< TextElement*(*)>(_a[1]))); break;
        case 1: onEditingFinished((*reinterpret_cast< TextElement*(*)>(_a[1]))); break;
        case 2: onTextChanged((*reinterpret_cast< TextElement*(*)>(_a[1]))); break;
        case 3: onTextUndoAdded((*reinterpret_cast< TextElement*(*)>(_a[1]))); break;
        case 4: onEditingElementDestroyed((*reinterpret_cast< QObject*(*)>(_a[1]))); break;
        case 5: onCursorPointerDestroyed(); break;
        case 6: onCanUndoChanged((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 7: onCanRedoChanged((*reinterpret_cast< bool(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 8;
    }
    return _id;
}
static const uint qt_meta_data_Velasquez__EmptyElementRemover[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      32,   31,   31,   31, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_Velasquez__EmptyElementRemover[] = {
    "Velasquez::EmptyElementRemover\0\0"
    "onDestroyed()\0"
};

const QMetaObject Velasquez::EmptyElementRemover::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_Velasquez__EmptyElementRemover,
      qt_meta_data_Velasquez__EmptyElementRemover, 0 }
};

const QMetaObject *Velasquez::EmptyElementRemover::metaObject() const
{
    return &staticMetaObject;
}

void *Velasquez::EmptyElementRemover::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Velasquez__EmptyElementRemover))
        return static_cast<void*>(const_cast< EmptyElementRemover*>(this));
    return QObject::qt_metacast(_clname);
}

int Velasquez::EmptyElementRemover::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onDestroyed(); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
