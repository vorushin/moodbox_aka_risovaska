/****************************************************************************
** Meta object code from reading C++ file 'setupdialog.h'
**
** Created: Wed Jun 24 21:01:20 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "setupdialog.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'setupdialog.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__SetupDialog[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       8,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      35,   22,   21,   21, 0x05,
      59,   21,   21,   21, 0x05,

 // slots: signature, parameters, type, tag, flags
      85,   76,   21,   21, 0x08,
     123,   21,   21,   21, 0x08,
     139,   21,   21,   21, 0x08,
     161,   21,   21,   21, 0x08,
     178,   21,   21,   21, 0x08,
     197,   21,   21,   21, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__SetupDialog[] = {
    "MoodBox::SetupDialog\0\0enableSounds\0"
    "soundStateChanged(bool)\0historyCleared()\0"
    "tabIndex\0on_setupTabWidget_currentChanged(int)\0"
    "onStartUpdate()\0onProgressCancelled()\0"
    "onCancelUpdate()\0onProfileUpdated()\0"
    "onSettingsUpdated()\0"
};

const QMetaObject MoodBox::SetupDialog::staticMetaObject = {
    { &MoodBoxDialog::staticMetaObject, qt_meta_stringdata_MoodBox__SetupDialog,
      qt_meta_data_MoodBox__SetupDialog, 0 }
};

const QMetaObject *MoodBox::SetupDialog::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::SetupDialog::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__SetupDialog))
        return static_cast<void*>(const_cast< SetupDialog*>(this));
    if (!strcmp(_clname, "SetupDialogClass"))
        return static_cast< SetupDialogClass*>(const_cast< SetupDialog*>(this));
    return MoodBoxDialog::qt_metacast(_clname);
}

int MoodBox::SetupDialog::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = MoodBoxDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: soundStateChanged((*reinterpret_cast< const bool(*)>(_a[1]))); break;
        case 1: historyCleared(); break;
        case 2: on_setupTabWidget_currentChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 3: onStartUpdate(); break;
        case 4: onProgressCancelled(); break;
        case 5: onCancelUpdate(); break;
        case 6: onProfileUpdated(); break;
        case 7: onSettingsUpdated(); break;
        default: ;
        }
        _id -= 8;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::SetupDialog::soundStateChanged(const bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::SetupDialog::historyCleared()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}
QT_END_MOC_NAMESPACE
