/****************************************************************************
** Meta object code from reading C++ file 'historyitemlist.h'
**
** Created: Tue May 5 10:34:43 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "historyitemlist.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'historyitemlist.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__HistoryItemList[] = {

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
      38,   25,   25,   25, 0x05,
      63,   57,   25,   25, 0x05,

 // slots: signature, parameters, type, tag, flags
     110,   90,   25,   25, 0x09,
     175,  158,   25,   25, 0x09,
     215,   25,   25,   25, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__HistoryItemList[] = {
    "MoodBox::HistoryItemList\0\0mouseLeft()\0"
    "selectionChanged()\0index\0"
    "itemActivated(QModelIndex)\0"
    "selected,deselected\0"
    "selectionChanged(QItemSelection,QItemSelection)\0"
    "current,previous\0"
    "currentChanged(QModelIndex,QModelIndex)\0"
    "updateGeometries()\0"
};

const QMetaObject MoodBox::HistoryItemList::staticMetaObject = {
    { &QListView::staticMetaObject, qt_meta_stringdata_MoodBox__HistoryItemList,
      qt_meta_data_MoodBox__HistoryItemList, 0 }
};

const QMetaObject *MoodBox::HistoryItemList::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::HistoryItemList::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__HistoryItemList))
        return static_cast<void*>(const_cast< HistoryItemList*>(this));
    return QListView::qt_metacast(_clname);
}

int MoodBox::HistoryItemList::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QListView::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: mouseLeft(); break;
        case 1: selectionChanged(); break;
        case 2: itemActivated((*reinterpret_cast< const QModelIndex(*)>(_a[1]))); break;
        case 3: selectionChanged((*reinterpret_cast< const QItemSelection(*)>(_a[1])),(*reinterpret_cast< const QItemSelection(*)>(_a[2]))); break;
        case 4: currentChanged((*reinterpret_cast< const QModelIndex(*)>(_a[1])),(*reinterpret_cast< const QModelIndex(*)>(_a[2]))); break;
        case 5: updateGeometries(); break;
        default: ;
        }
        _id -= 6;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::HistoryItemList::mouseLeft()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void MoodBox::HistoryItemList::selectionChanged()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void MoodBox::HistoryItemList::itemActivated(const QModelIndex & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}
QT_END_MOC_NAMESPACE
