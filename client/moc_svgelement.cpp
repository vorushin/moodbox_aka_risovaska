/****************************************************************************
** Meta object code from reading C++ file 'svgelement.h'
**
** Created: Tue May 5 10:35:03 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../Velasquez/Qt/svgelement.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'svgelement.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Velasquez__SvgElement[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      23,   22,   22,   22, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_Velasquez__SvgElement[] = {
    "Velasquez::SvgElement\0\0onRepaintNeeded()\0"
};

const QMetaObject Velasquez::SvgElement::staticMetaObject = {
    { &TransformableElement::staticMetaObject, qt_meta_stringdata_Velasquez__SvgElement,
      qt_meta_data_Velasquez__SvgElement, 0 }
};

const QMetaObject *Velasquez::SvgElement::metaObject() const
{
    return &staticMetaObject;
}

void *Velasquez::SvgElement::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Velasquez__SvgElement))
        return static_cast<void*>(const_cast< SvgElement*>(this));
    return TransformableElement::qt_metacast(_clname);
}

int Velasquez::SvgElement::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = TransformableElement::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onRepaintNeeded(); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
