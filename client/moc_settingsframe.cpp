/****************************************************************************
** Meta object code from reading C++ file 'settingsframe.h'
**
** Created: Wed Jun 24 21:01:19 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "settingsframe.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'settingsframe.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__SettingsFrame[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       8,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      24,   23,   23,   23, 0x05,

 // slots: signature, parameters, type, tag, flags
      41,   23,   23,   23, 0x08,
      88,   79,   23,   23, 0x08,
     126,   79,   23,   23, 0x08,
     189,  183,   23,   23, 0x08,
     237,  232,   23,   23, 0x08,
     274,   23,   23,   23, 0x08,
     314,   23,   23,   23, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__SettingsFrame[] = {
    "MoodBox::SettingsFrame\0\0historyCleared()\0"
    "on_checkForNewVersionButton_clicked()\0"
    "newState\0on_useProxyCheckBox_stateChanged(int)\0"
    "on_proxyRequiresAuthenticationCheckBox_stateChanged(int)\0"
    "index\0on_proxyTypeCombo_currentIndexChanged(int)\0"
    "text\0on_proxyPortEdit_textEdited(QString)\0"
    "on_setStandardProxyPortButton_clicked()\0"
    "on_clearHistoryButton_clicked()\0"
};

const QMetaObject MoodBox::SettingsFrame::staticMetaObject = {
    { &SetupDialogFrame::staticMetaObject, qt_meta_stringdata_MoodBox__SettingsFrame,
      qt_meta_data_MoodBox__SettingsFrame, 0 }
};

const QMetaObject *MoodBox::SettingsFrame::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::SettingsFrame::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__SettingsFrame))
        return static_cast<void*>(const_cast< SettingsFrame*>(this));
    if (!strcmp(_clname, "SettingsFrameClass"))
        return static_cast< SettingsFrameClass*>(const_cast< SettingsFrame*>(this));
    return SetupDialogFrame::qt_metacast(_clname);
}

int MoodBox::SettingsFrame::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = SetupDialogFrame::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: historyCleared(); break;
        case 1: on_checkForNewVersionButton_clicked(); break;
        case 2: on_useProxyCheckBox_stateChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 3: on_proxyRequiresAuthenticationCheckBox_stateChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 4: on_proxyTypeCombo_currentIndexChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 5: on_proxyPortEdit_textEdited((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 6: on_setStandardProxyPortButton_clicked(); break;
        case 7: on_clearHistoryButton_clicked(); break;
        default: ;
        }
        _id -= 8;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::SettingsFrame::historyCleared()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}
QT_END_MOC_NAMESPACE
