/****************************************************************************
** Meta object code from reading C++ file 'newcontactlistwindow.h'
**
** Created: Wed Jun 24 21:01:15 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "newcontactlistwindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'newcontactlistwindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__NewContactListWindow[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      17,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      34,   31,   30,   30, 0x05,
      67,   58,   30,   30, 0x05,
      99,   30,   30,   30, 0x05,
     119,   30,   30,   30, 0x05,
     130,   30,   30,   30, 0x05,
     142,   30,   30,   30, 0x05,
     153,   30,   30,   30, 0x05,
     175,  162,   30,   30, 0x05,
     199,   30,   30,   30, 0x05,

 // slots: signature, parameters, type, tag, flags
     216,   30,   30,   30, 0x0a,
     238,   30,   30,   30, 0x0a,
     257,   30,   30,   30, 0x0a,
     298,  287,   30,   30, 0x0a,
     351,  346,   30,   30, 0x0a,
     379,  371,   30,   30, 0x08,
     412,  371,   30,   30, 0x08,
     440,   30,   30,   30, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__NewContactListWindow[] = {
    "MoodBox::NewContactListWindow\0\0id\0"
    "contactSelected(qint32)\0id,image\0"
    "contactImageDrop(qint32,QImage)\0"
    "unreadContacts(int)\0goOnline()\0"
    "goOffline()\0goLogout()\0goExit()\0"
    "enableSounds\0soundStateChanged(bool)\0"
    "historyCleared()\0onShowProfileDialog()\0"
    "onShowFindDialog()\0onShowNetworkSettingsDialog()\0"
    "userStatus\0onUserStatusChanged(UserStatus::UserStatusEnum)\0"
    "newX\0onResizeNeeded(int)\0checked\0"
    "on_setupToolButton_toggled(bool)\0"
    "on_findButton_toggled(bool)\0"
    "onStatusButtonClicked()\0"
};

const QMetaObject MoodBox::NewContactListWindow::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_MoodBox__NewContactListWindow,
      qt_meta_data_MoodBox__NewContactListWindow, 0 }
};

const QMetaObject *MoodBox::NewContactListWindow::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::NewContactListWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__NewContactListWindow))
        return static_cast<void*>(const_cast< NewContactListWindow*>(this));
    if (!strcmp(_clname, "NewContactListWindowClass"))
        return static_cast< NewContactListWindowClass*>(const_cast< NewContactListWindow*>(this));
    return QWidget::qt_metacast(_clname);
}

int MoodBox::NewContactListWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: contactSelected((*reinterpret_cast< qint32(*)>(_a[1]))); break;
        case 1: contactImageDrop((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const QImage(*)>(_a[2]))); break;
        case 2: unreadContacts((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 3: goOnline(); break;
        case 4: goOffline(); break;
        case 5: goLogout(); break;
        case 6: goExit(); break;
        case 7: soundStateChanged((*reinterpret_cast< const bool(*)>(_a[1]))); break;
        case 8: historyCleared(); break;
        case 9: onShowProfileDialog(); break;
        case 10: onShowFindDialog(); break;
        case 11: onShowNetworkSettingsDialog(); break;
        case 12: onUserStatusChanged((*reinterpret_cast< UserStatus::UserStatusEnum(*)>(_a[1]))); break;
        case 13: onResizeNeeded((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 14: on_setupToolButton_toggled((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 15: on_findButton_toggled((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 16: onStatusButtonClicked(); break;
        default: ;
        }
        _id -= 17;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::NewContactListWindow::contactSelected(qint32 _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::NewContactListWindow::contactImageDrop(qint32 _t1, const QImage & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void MoodBox::NewContactListWindow::unreadContacts(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void MoodBox::NewContactListWindow::goOnline()
{
    QMetaObject::activate(this, &staticMetaObject, 3, 0);
}

// SIGNAL 4
void MoodBox::NewContactListWindow::goOffline()
{
    QMetaObject::activate(this, &staticMetaObject, 4, 0);
}

// SIGNAL 5
void MoodBox::NewContactListWindow::goLogout()
{
    QMetaObject::activate(this, &staticMetaObject, 5, 0);
}

// SIGNAL 6
void MoodBox::NewContactListWindow::goExit()
{
    QMetaObject::activate(this, &staticMetaObject, 6, 0);
}

// SIGNAL 7
void MoodBox::NewContactListWindow::soundStateChanged(const bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 7, _a);
}

// SIGNAL 8
void MoodBox::NewContactListWindow::historyCleared()
{
    QMetaObject::activate(this, &staticMetaObject, 8, 0);
}
QT_END_MOC_NAMESPACE
