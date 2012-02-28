/****************************************************************************
** Meta object code from reading C++ file 'clipartentrieslist.h'
**
** Created: Tue May 5 10:34:37 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "clipartentrieslist.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'clipartentrieslist.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__ClipartEntriesList[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      37,   29,   28,   28, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ClipartEntriesList[] = {
    "MoodBox::ClipartEntriesList\0\0entries\0"
    "loadEntries(QList<ItemImages>)\0"
};

const QMetaObject MoodBox::ClipartEntriesList::staticMetaObject = {
    { &QListView::staticMetaObject, qt_meta_stringdata_MoodBox__ClipartEntriesList,
      qt_meta_data_MoodBox__ClipartEntriesList, 0 }
};

const QMetaObject *MoodBox::ClipartEntriesList::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ClipartEntriesList::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ClipartEntriesList))
        return static_cast<void*>(const_cast< ClipartEntriesList*>(this));
    return QListView::qt_metacast(_clname);
}

int MoodBox::ClipartEntriesList::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QListView::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: loadEntries((*reinterpret_cast< const QList<ItemImages>(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
