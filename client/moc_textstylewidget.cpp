/****************************************************************************
** Meta object code from reading C++ file 'textstylewidget.h'
**
** Created: Tue May 5 10:34:55 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "textstylewidget.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'textstylewidget.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__TextStyleWidget[] = {

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
      61,   25,   25,   25, 0x05,

 // slots: signature, parameters, type, tag, flags
      77,   70,   25,   25, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__TextStyleWidget[] = {
    "MoodBox::TextStyleWidget\0\0fontName\0"
    "fontNameSelected(QString)\0closed()\0"
    "action\0onStyleActionTriggered(QAction*)\0"
};

const QMetaObject MoodBox::TextStyleWidget::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_MoodBox__TextStyleWidget,
      qt_meta_data_MoodBox__TextStyleWidget, 0 }
};

const QMetaObject *MoodBox::TextStyleWidget::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::TextStyleWidget::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__TextStyleWidget))
        return static_cast<void*>(const_cast< TextStyleWidget*>(this));
    if (!strcmp(_clname, "TextStyleWidgetClass"))
        return static_cast< TextStyleWidgetClass*>(const_cast< TextStyleWidget*>(this));
    return QWidget::qt_metacast(_clname);
}

int MoodBox::TextStyleWidget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: fontNameSelected((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 1: closed(); break;
        case 2: onStyleActionTriggered((*reinterpret_cast< QAction*(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::TextStyleWidget::fontNameSelected(const QString & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::TextStyleWidget::closed()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}
QT_END_MOC_NAMESPACE
