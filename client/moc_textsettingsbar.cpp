/****************************************************************************
** Meta object code from reading C++ file 'textsettingsbar.h'
**
** Created: Tue May 5 10:34:55 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "textsettingsbar.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'textsettingsbar.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__TextSettingsBar[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       9,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      32,   26,   25,   25, 0x08,
      71,   56,   25,   25, 0x08,
     110,  101,   25,   25, 0x08,
     146,  138,   25,   25, 0x08,
     173,   25,   25,   25, 0x08,
     197,   25,   25,   25, 0x08,
     223,   25,   25,   25, 0x08,
     260,  252,   25,   25, 0x08,
     298,   25,   25,   25, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__TextSettingsBar[] = {
    "MoodBox::TextSettingsBar\0\0index\0"
    "onColorSelected(qint32)\0index,newColor\0"
    "onColorChanged(qint32,QColor)\0fontName\0"
    "onFontNameSelected(QString)\0palette\0"
    "onPaletteSelected(Palette)\0"
    "on_boldButton_clicked()\0"
    "on_italicButton_clicked()\0"
    "on_underlineButton_clicked()\0checked\0"
    "on_paletteManagerButton_toggled(bool)\0"
    "onPaletteManagerFinished()\0"
};

const QMetaObject MoodBox::TextSettingsBar::staticMetaObject = {
    { &ToolSettingsBar::staticMetaObject, qt_meta_stringdata_MoodBox__TextSettingsBar,
      qt_meta_data_MoodBox__TextSettingsBar, 0 }
};

const QMetaObject *MoodBox::TextSettingsBar::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::TextSettingsBar::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__TextSettingsBar))
        return static_cast<void*>(const_cast< TextSettingsBar*>(this));
    if (!strcmp(_clname, "TextSettingsBarClass"))
        return static_cast< TextSettingsBarClass*>(const_cast< TextSettingsBar*>(this));
    return ToolSettingsBar::qt_metacast(_clname);
}

int MoodBox::TextSettingsBar::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ToolSettingsBar::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onColorSelected((*reinterpret_cast< qint32(*)>(_a[1]))); break;
        case 1: onColorChanged((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const QColor(*)>(_a[2]))); break;
        case 2: onFontNameSelected((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 3: onPaletteSelected((*reinterpret_cast< const Palette(*)>(_a[1]))); break;
        case 4: on_boldButton_clicked(); break;
        case 5: on_italicButton_clicked(); break;
        case 6: on_underlineButton_clicked(); break;
        case 7: on_paletteManagerButton_toggled((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 8: onPaletteManagerFinished(); break;
        default: ;
        }
        _id -= 9;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
