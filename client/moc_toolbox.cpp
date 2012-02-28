/****************************************************************************
** Meta object code from reading C++ file 'toolbox.h'
**
** Created: Tue May 5 10:35:04 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "../../Velasquez/Qt/toolbox.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'toolbox.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Velasquez__ToolBox[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      25,   20,   19,   19, 0x05,
      53,   19,   19,   19, 0x05,
      67,   19,   19,   19, 0x05,

 // slots: signature, parameters, type, tag, flags
      82,   19,   19,   19, 0x0a,
     105,   19,   19,   19, 0x0a,
     126,   19,   19,   19, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_Velasquez__ToolBox[] = {
    "Velasquez::ToolBox\0\0tool\0"
    "toolActivated(DrawingTool*)\0copyRequest()\0"
    "pasteRequest()\0copyImageToClipboard()\0"
    "pasteFromClipboard()\0onSceneCleared()\0"
};

const QMetaObject Velasquez::ToolBox::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_Velasquez__ToolBox,
      qt_meta_data_Velasquez__ToolBox, 0 }
};

const QMetaObject *Velasquez::ToolBox::metaObject() const
{
    return &staticMetaObject;
}

void *Velasquez::ToolBox::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Velasquez__ToolBox))
        return static_cast<void*>(const_cast< ToolBox*>(this));
    return QObject::qt_metacast(_clname);
}

int Velasquez::ToolBox::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: toolActivated((*reinterpret_cast< DrawingTool*(*)>(_a[1]))); break;
        case 1: copyRequest(); break;
        case 2: pasteRequest(); break;
        case 3: copyImageToClipboard(); break;
        case 4: pasteFromClipboard(); break;
        case 5: onSceneCleared(); break;
        default: ;
        }
        _id -= 6;
    }
    return _id;
}

// SIGNAL 0
void Velasquez::ToolBox::toolActivated(DrawingTool * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void Velasquez::ToolBox::copyRequest()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void Velasquez::ToolBox::pasteRequest()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}
QT_END_MOC_NAMESPACE
