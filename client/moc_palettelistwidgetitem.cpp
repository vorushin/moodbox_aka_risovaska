/****************************************************************************
** Meta object code from reading C++ file 'palettelistwidgetitem.h'
**
** Created: Tue May 5 10:34:49 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "palettelistwidgetitem.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'palettelistwidgetitem.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__PaletteListWidgetItem[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      37,   32,   31,   31, 0x05,
      69,   32,   31,   31, 0x05,

 // slots: signature, parameters, type, tag, flags
     109,   31,   31,   31, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__PaletteListWidgetItem[] = {
    "MoodBox::PaletteListWidgetItem\0\0item\0"
    "clicked(PaletteListWidgetItem*)\0"
    "actionActivated(PaletteListWidgetItem*)\0"
    "onActionActivated()\0"
};

const QMetaObject MoodBox::PaletteListWidgetItem::staticMetaObject = {
    { &QFrame::staticMetaObject, qt_meta_stringdata_MoodBox__PaletteListWidgetItem,
      qt_meta_data_MoodBox__PaletteListWidgetItem, 0 }
};

const QMetaObject *MoodBox::PaletteListWidgetItem::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::PaletteListWidgetItem::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__PaletteListWidgetItem))
        return static_cast<void*>(const_cast< PaletteListWidgetItem*>(this));
    if (!strcmp(_clname, "PaletteListWidgetItemClass"))
        return static_cast< PaletteListWidgetItemClass*>(const_cast< PaletteListWidgetItem*>(this));
    return QFrame::qt_metacast(_clname);
}

int MoodBox::PaletteListWidgetItem::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QFrame::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: clicked((*reinterpret_cast< PaletteListWidgetItem*(*)>(_a[1]))); break;
        case 1: actionActivated((*reinterpret_cast< PaletteListWidgetItem*(*)>(_a[1]))); break;
        case 2: onActionActivated(); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::PaletteListWidgetItem::clicked(PaletteListWidgetItem * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::PaletteListWidgetItem::actionActivated(PaletteListWidgetItem * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
QT_END_MOC_NAMESPACE
