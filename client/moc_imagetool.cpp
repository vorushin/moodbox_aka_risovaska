/****************************************************************************
** Meta object code from reading C++ file 'imagetool.h'
**
** Created: Wed Jun 24 21:01:26 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../Velasquez/Qt/imagetool.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'imagetool.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Velasquez__ImageTool[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      28,   22,   21,   21, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_Velasquez__ImageTool[] = {
    "Velasquez::ImageTool\0\0reply\0"
    "onImageLoadReply(QNetworkReply*)\0"
};

const QMetaObject Velasquez::ImageTool::staticMetaObject = {
    { &TransformableTool::staticMetaObject, qt_meta_stringdata_Velasquez__ImageTool,
      qt_meta_data_Velasquez__ImageTool, 0 }
};

const QMetaObject *Velasquez::ImageTool::metaObject() const
{
    return &staticMetaObject;
}

void *Velasquez::ImageTool::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Velasquez__ImageTool))
        return static_cast<void*>(const_cast< ImageTool*>(this));
    return TransformableTool::qt_metacast(_clname);
}

int Velasquez::ImageTool::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = TransformableTool::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onImageLoadReply((*reinterpret_cast< QNetworkReply*(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
