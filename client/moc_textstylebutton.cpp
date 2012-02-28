/****************************************************************************
** Meta object code from reading C++ file 'textstylebutton.h'
**
** Created: Tue May 5 10:34:55 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "textstylebutton.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'textstylebutton.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__TextStyleButton[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      35,   26,   25,   25, 0x05,

 // slots: signature, parameters, type, tag, flags
      69,   61,   25,   25, 0x08,
      85,   25,   25,   25, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__TextStyleButton[] = {
    "MoodBox::TextStyleButton\0\0fontName\0"
    "fontNameSelected(QString)\0toggled\0"
    "onToggled(bool)\0onStyleWidgetClosed()\0"
};

const QMetaObject MoodBox::TextStyleButton::staticMetaObject = {
    { &QToolButton::staticMetaObject, qt_meta_stringdata_MoodBox__TextStyleButton,
      qt_meta_data_MoodBox__TextStyleButton, 0 }
};

const QMetaObject *MoodBox::TextStyleButton::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::TextStyleButton::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__TextStyleButton))
        return static_cast<void*>(const_cast< TextStyleButton*>(this));
    return QToolButton::qt_metacast(_clname);
}

int MoodBox::TextStyleButton::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QToolButton::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: fontNameSelected((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 1: onToggled((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 2: onStyleWidgetClosed(); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::TextStyleButton::fontNameSelected(const QString & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
