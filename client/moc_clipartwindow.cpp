/****************************************************************************
** Meta object code from reading C++ file 'clipartwindow.h'
**
** Created: Wed Jun 24 21:01:08 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "clipartwindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'clipartwindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__ClipartWindow[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      29,   24,   23,   23, 0x0a,
      76,   68,   23,   23, 0x09,
     101,   23,   23,   23, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ClipartWindow[] = {
    "MoodBox::ClipartWindow\0\0page\0"
    "showPage(ClipartPage::ClipartPageEnum)\0"
    "palette\0fillBackgrounds(Palette)\0"
    "on_browseButton_clicked()\0"
};

const QMetaObject MoodBox::ClipartWindow::staticMetaObject = {
    { &MovableWidget::staticMetaObject, qt_meta_stringdata_MoodBox__ClipartWindow,
      qt_meta_data_MoodBox__ClipartWindow, 0 }
};

const QMetaObject *MoodBox::ClipartWindow::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ClipartWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ClipartWindow))
        return static_cast<void*>(const_cast< ClipartWindow*>(this));
    if (!strcmp(_clname, "ClipartWindowClass"))
        return static_cast< ClipartWindowClass*>(const_cast< ClipartWindow*>(this));
    return MovableWidget::qt_metacast(_clname);
}

int MoodBox::ClipartWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = MovableWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: showPage((*reinterpret_cast< ClipartPage::ClipartPageEnum(*)>(_a[1]))); break;
        case 1: fillBackgrounds((*reinterpret_cast< const Palette(*)>(_a[1]))); break;
        case 2: on_browseButton_clicked(); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
