/****************************************************************************
** Meta object code from reading C++ file 'authticketupdater.h'
**
** Created: Wed Jun 24 21:01:06 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "authticketupdater.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'authticketupdater.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__AuthTicketUpdater[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      28,   27,   27,   27, 0x09,
      67,   48,   27,   27, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__AuthTicketUpdater[] = {
    "MoodBox::AuthTicketUpdater\0\0"
    "onAuthUpdateTimer()\0state,fault,result\0"
    "onAuthUpdateRetry(QVariant,Fault,AuthTicketResult)\0"
};

const QMetaObject MoodBox::AuthTicketUpdater::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MoodBox__AuthTicketUpdater,
      qt_meta_data_MoodBox__AuthTicketUpdater, 0 }
};

const QMetaObject *MoodBox::AuthTicketUpdater::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::AuthTicketUpdater::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__AuthTicketUpdater))
        return static_cast<void*>(const_cast< AuthTicketUpdater*>(this));
    return QObject::qt_metacast(_clname);
}

int MoodBox::AuthTicketUpdater::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onAuthUpdateTimer(); break;
        case 1: onAuthUpdateRetry((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< AuthTicketResult(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
