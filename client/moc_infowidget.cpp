/****************************************************************************
** Meta object code from reading C++ file 'infowidget.h'
**
** Created: Tue May 5 10:34:44 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "infowidget.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'infowidget.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__InfoWidget[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      21,   20,   20,   20, 0x05,

 // slots: signature, parameters, type, tag, flags
      32,   20,   20,   20, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__InfoWidget[] = {
    "MoodBox::InfoWidget\0\0finished()\0"
    "onFinishLinkAction()\0"
};

const QMetaObject MoodBox::InfoWidget::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_MoodBox__InfoWidget,
      qt_meta_data_MoodBox__InfoWidget, 0 }
};

const QMetaObject *MoodBox::InfoWidget::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::InfoWidget::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__InfoWidget))
        return static_cast<void*>(const_cast< InfoWidget*>(this));
    if (!strcmp(_clname, "InfoWidgetClass"))
        return static_cast< InfoWidgetClass*>(const_cast< InfoWidget*>(this));
    return QWidget::qt_metacast(_clname);
}

int MoodBox::InfoWidget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: finished(); break;
        case 1: onFinishLinkAction(); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::InfoWidget::finished()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}
QT_END_MOC_NAMESPACE
