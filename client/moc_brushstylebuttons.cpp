/****************************************************************************
** Meta object code from reading C++ file 'brushstylebuttons.h'
**
** Created: Tue May 5 10:34:35 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "brushstylebuttons.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'brushstylebuttons.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__BrushStyleButton[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      33,   27,   26,   26, 0x0a,
      58,   50,   26,   26, 0x09,
      74,   26,   26,   26, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__BrushStyleButton[] = {
    "MoodBox::BrushStyleButton\0\0color\0"
    "setColor(QColor)\0toggled\0onToggled(bool)\0"
    "onStyleWidgetClosed()\0"
};

const QMetaObject MoodBox::BrushStyleButton::staticMetaObject = {
    { &QToolButton::staticMetaObject, qt_meta_stringdata_MoodBox__BrushStyleButton,
      qt_meta_data_MoodBox__BrushStyleButton, 0 }
};

const QMetaObject *MoodBox::BrushStyleButton::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::BrushStyleButton::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__BrushStyleButton))
        return static_cast<void*>(const_cast< BrushStyleButton*>(this));
    return QToolButton::qt_metacast(_clname);
}

int MoodBox::BrushStyleButton::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QToolButton::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: setColor((*reinterpret_cast< const QColor(*)>(_a[1]))); break;
        case 1: onToggled((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 2: onStyleWidgetClosed(); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}
static const uint qt_meta_data_MoodBox__BrushSizeButton[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      36,   26,   25,   25, 0x05,

 // slots: signature, parameters, type, tag, flags
      60,   54,   25,   25, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__BrushSizeButton[] = {
    "MoodBox::BrushSizeButton\0\0sizeIndex\0"
    "sizeSelected(int)\0index\0setSizeIndex(int)\0"
};

const QMetaObject MoodBox::BrushSizeButton::staticMetaObject = {
    { &BrushStyleButton::staticMetaObject, qt_meta_stringdata_MoodBox__BrushSizeButton,
      qt_meta_data_MoodBox__BrushSizeButton, 0 }
};

const QMetaObject *MoodBox::BrushSizeButton::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::BrushSizeButton::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__BrushSizeButton))
        return static_cast<void*>(const_cast< BrushSizeButton*>(this));
    return BrushStyleButton::qt_metacast(_clname);
}

int MoodBox::BrushSizeButton::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = BrushStyleButton::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: sizeSelected((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 1: setSizeIndex((*reinterpret_cast< int(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::BrushSizeButton::sizeSelected(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__BrushAlphaButton[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      38,   27,   26,   26, 0x05,

 // slots: signature, parameters, type, tag, flags
      63,   57,   26,   26, 0x0a,
      82,   57,   26,   26, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__BrushAlphaButton[] = {
    "MoodBox::BrushAlphaButton\0\0alphaIndex\0"
    "alphaSelected(int)\0index\0setAlphaIndex(int)\0"
    "setSizeIndex(int)\0"
};

const QMetaObject MoodBox::BrushAlphaButton::staticMetaObject = {
    { &BrushStyleButton::staticMetaObject, qt_meta_stringdata_MoodBox__BrushAlphaButton,
      qt_meta_data_MoodBox__BrushAlphaButton, 0 }
};

const QMetaObject *MoodBox::BrushAlphaButton::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::BrushAlphaButton::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__BrushAlphaButton))
        return static_cast<void*>(const_cast< BrushAlphaButton*>(this));
    return BrushStyleButton::qt_metacast(_clname);
}

int MoodBox::BrushAlphaButton::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = BrushStyleButton::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: alphaSelected((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 1: setAlphaIndex((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 2: setSizeIndex((*reinterpret_cast< int(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::BrushAlphaButton::alphaSelected(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
