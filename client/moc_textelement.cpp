/****************************************************************************
** Meta object code from reading C++ file 'textelement.h'
**
** Created: Tue May 5 10:35:03 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../Velasquez/Qt/textelement.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'textelement.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Velasquez__TextElement[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      11,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      32,   24,   23,   23, 0x05,
      61,   24,   23,   23, 0x05,
      91,   24,   23,   23, 0x05,
     117,   24,   23,   23, 0x05,
     145,   24,   23,   23, 0x05,

 // slots: signature, parameters, type, tag, flags
     176,  171,   23,   23, 0x08,
     200,  195,   23,   23, 0x08,
     224,  195,   23,   23, 0x08,
     252,   23,   23,   23, 0x08,
     268,   23,   23,   23, 0x08,
     289,   23,   23,   23, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_Velasquez__TextElement[] = {
    "Velasquez::TextElement\0\0element\0"
    "editingStarted(TextElement*)\0"
    "editingFinished(TextElement*)\0"
    "textChanged(TextElement*)\0"
    "textUndoAdded(TextElement*)\0"
    "cursorMoved(TextElement*)\0size\0"
    "updateRect(QSizeF)\0rect\0onUpdateRequest(QRectF)\0"
    "onVisibilityRequest(QRectF)\0onTextChanged()\0"
    "onUndoCommandAdded()\0onCursorPositionChanged()\0"
};

const QMetaObject Velasquez::TextElement::staticMetaObject = {
    { &TransformableElement::staticMetaObject, qt_meta_stringdata_Velasquez__TextElement,
      qt_meta_data_Velasquez__TextElement, 0 }
};

const QMetaObject *Velasquez::TextElement::metaObject() const
{
    return &staticMetaObject;
}

void *Velasquez::TextElement::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Velasquez__TextElement))
        return static_cast<void*>(const_cast< TextElement*>(this));
    return TransformableElement::qt_metacast(_clname);
}

int Velasquez::TextElement::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = TransformableElement::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: editingStarted((*reinterpret_cast< TextElement*(*)>(_a[1]))); break;
        case 1: editingFinished((*reinterpret_cast< TextElement*(*)>(_a[1]))); break;
        case 2: textChanged((*reinterpret_cast< TextElement*(*)>(_a[1]))); break;
        case 3: textUndoAdded((*reinterpret_cast< TextElement*(*)>(_a[1]))); break;
        case 4: cursorMoved((*reinterpret_cast< TextElement*(*)>(_a[1]))); break;
        case 5: updateRect((*reinterpret_cast< const QSizeF(*)>(_a[1]))); break;
        case 6: onUpdateRequest((*reinterpret_cast< const QRectF(*)>(_a[1]))); break;
        case 7: onVisibilityRequest((*reinterpret_cast< const QRectF(*)>(_a[1]))); break;
        case 8: onTextChanged(); break;
        case 9: onUndoCommandAdded(); break;
        case 10: onCursorPositionChanged(); break;
        default: ;
        }
        _id -= 11;
    }
    return _id;
}

// SIGNAL 0
void Velasquez::TextElement::editingStarted(TextElement * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void Velasquez::TextElement::editingFinished(TextElement * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void Velasquez::TextElement::textChanged(TextElement * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void Velasquez::TextElement::textUndoAdded(TextElement * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}

// SIGNAL 4
void Velasquez::TextElement::cursorMoved(TextElement * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 4, _a);
}
QT_END_MOC_NAMESPACE
