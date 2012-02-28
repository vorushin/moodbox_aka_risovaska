/****************************************************************************
** Meta object code from reading C++ file 'contactlistmenu.h'
**
** Created: Tue May 5 10:34:39 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "contactlistmenu.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'contactlistmenu.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__ContactListMenu[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      26,   25,   25,   25, 0x05,
      44,   25,   25,   25, 0x05,
      62,   25,   25,   25, 0x05,
      73,   25,   25,   25, 0x05,
      85,   25,   25,   25, 0x05,
      96,   25,   25,   25, 0x05,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ContactListMenu[] = {
    "MoodBox::ContactListMenu\0\0menuAboutToShow()\0"
    "menuAboutToHide()\0goOnline()\0goOffline()\0"
    "goLogout()\0goExit()\0"
};

const QMetaObject MoodBox::ContactListMenu::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_MoodBox__ContactListMenu,
      qt_meta_data_MoodBox__ContactListMenu, 0 }
};

const QMetaObject *MoodBox::ContactListMenu::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ContactListMenu::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ContactListMenu))
        return static_cast<void*>(const_cast< ContactListMenu*>(this));
    if (!strcmp(_clname, "ContactListMenuClass"))
        return static_cast< ContactListMenuClass*>(const_cast< ContactListMenu*>(this));
    return QWidget::qt_metacast(_clname);
}

int MoodBox::ContactListMenu::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: menuAboutToShow(); break;
        case 1: menuAboutToHide(); break;
        case 2: goOnline(); break;
        case 3: goOffline(); break;
        case 4: goLogout(); break;
        case 5: goExit(); break;
        default: ;
        }
        _id -= 6;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::ContactListMenu::menuAboutToShow()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void MoodBox::ContactListMenu::menuAboutToHide()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void MoodBox::ContactListMenu::goOnline()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}

// SIGNAL 3
void MoodBox::ContactListMenu::goOffline()
{
    QMetaObject::activate(this, &staticMetaObject, 3, 0);
}

// SIGNAL 4
void MoodBox::ContactListMenu::goLogout()
{
    QMetaObject::activate(this, &staticMetaObject, 4, 0);
}

// SIGNAL 5
void MoodBox::ContactListMenu::goExit()
{
    QMetaObject::activate(this, &staticMetaObject, 5, 0);
}
QT_END_MOC_NAMESPACE
