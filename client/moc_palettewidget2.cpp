/****************************************************************************
** Meta object code from reading C++ file 'palettewidget2.h'
**
** Created: Tue May 5 10:34:49 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "palettewidget2.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'palettewidget2.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__PaletteWidget2[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      31,   25,   24,   24, 0x05,
      68,   53,   24,   24, 0x05,

 // slots: signature, parameters, type, tag, flags
     104,   96,   24,   24, 0x0a,
     133,  124,   24,   24, 0x0a,
     167,  161,   24,   24, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__PaletteWidget2[] = {
    "MoodBox::PaletteWidget2\0\0index\0"
    "colorSelected(qint32)\0index,newColor\0"
    "colorChanged(qint32,QColor)\0palette\0"
    "setPalette(Palette)\0newColor\0"
    "updateSelectedColor(QColor)\0color\0"
    "onColorSelected(Color)\0"
};

const QMetaObject MoodBox::PaletteWidget2::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_MoodBox__PaletteWidget2,
      qt_meta_data_MoodBox__PaletteWidget2, 0 }
};

const QMetaObject *MoodBox::PaletteWidget2::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::PaletteWidget2::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__PaletteWidget2))
        return static_cast<void*>(const_cast< PaletteWidget2*>(this));
    return QWidget::qt_metacast(_clname);
}

int MoodBox::PaletteWidget2::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: colorSelected((*reinterpret_cast< qint32(*)>(_a[1]))); break;
        case 1: colorChanged((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const QColor(*)>(_a[2]))); break;
        case 2: setPalette((*reinterpret_cast< const Palette(*)>(_a[1]))); break;
        case 3: updateSelectedColor((*reinterpret_cast< const QColor(*)>(_a[1]))); break;
        case 4: onColorSelected((*reinterpret_cast< const Color(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 5;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::PaletteWidget2::colorSelected(qint32 _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::PaletteWidget2::colorChanged(qint32 _t1, const QColor & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
QT_END_MOC_NAMESPACE
