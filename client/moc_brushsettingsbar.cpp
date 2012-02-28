/****************************************************************************
** Meta object code from reading C++ file 'brushsettingsbar.h'
**
** Created: Tue May 5 10:34:35 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "brushsettingsbar.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'brushsettingsbar.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__BrushSettingsBar[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       8,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      33,   27,   26,   26, 0x0a,
      67,   61,   26,   26, 0x08,
     106,   91,   26,   26, 0x08,
     146,  136,   26,   26, 0x08,
     177,  166,   26,   26, 0x08,
     206,  198,   26,   26, 0x08,
     241,  233,   26,   26, 0x08,
     279,   26,   26,   26, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__BrushSettingsBar[] = {
    "MoodBox::BrushSettingsBar\0\0color\0"
    "changeSelectedColor(QColor)\0index\0"
    "onColorSelected(qint32)\0index,newColor\0"
    "onColorChanged(qint32,QColor)\0sizeIndex\0"
    "onSizeSelected(int)\0alphaIndex\0"
    "onAlphaSelected(int)\0palette\0"
    "onPaletteSelected(Palette)\0checked\0"
    "on_paletteManagerButton_toggled(bool)\0"
    "onPaletteManagerFinished()\0"
};

const QMetaObject MoodBox::BrushSettingsBar::staticMetaObject = {
    { &ToolSettingsBar::staticMetaObject, qt_meta_stringdata_MoodBox__BrushSettingsBar,
      qt_meta_data_MoodBox__BrushSettingsBar, 0 }
};

const QMetaObject *MoodBox::BrushSettingsBar::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::BrushSettingsBar::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__BrushSettingsBar))
        return static_cast<void*>(const_cast< BrushSettingsBar*>(this));
    if (!strcmp(_clname, "BrushSettingsBarClass"))
        return static_cast< BrushSettingsBarClass*>(const_cast< BrushSettingsBar*>(this));
    return ToolSettingsBar::qt_metacast(_clname);
}

int MoodBox::BrushSettingsBar::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ToolSettingsBar::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: changeSelectedColor((*reinterpret_cast< const QColor(*)>(_a[1]))); break;
        case 1: onColorSelected((*reinterpret_cast< qint32(*)>(_a[1]))); break;
        case 2: onColorChanged((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const QColor(*)>(_a[2]))); break;
        case 3: onSizeSelected((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 4: onAlphaSelected((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 5: onPaletteSelected((*reinterpret_cast< const Palette(*)>(_a[1]))); break;
        case 6: on_paletteManagerButton_toggled((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 7: onPaletteManagerFinished(); break;
        default: ;
        }
        _id -= 8;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
