/****************************************************************************
** Meta object code from reading C++ file 'toolsettingsbar.h'
**
** Created: Tue May 5 10:34:55 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "toolsettingsbar.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'toolsettingsbar.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__ToolSettingsBar[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      29,   26,   25,   25, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ToolSettingsBar[] = {
    "MoodBox::ToolSettingsBar\0\0id\0"
    "onSettingChanged(qint32)\0"
};

const QMetaObject MoodBox::ToolSettingsBar::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_MoodBox__ToolSettingsBar,
      qt_meta_data_MoodBox__ToolSettingsBar, 0 }
};

const QMetaObject *MoodBox::ToolSettingsBar::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ToolSettingsBar::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ToolSettingsBar))
        return static_cast<void*>(const_cast< ToolSettingsBar*>(this));
    return QWidget::qt_metacast(_clname);
}

int MoodBox::ToolSettingsBar::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onSettingChanged((*reinterpret_cast< qint32(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
