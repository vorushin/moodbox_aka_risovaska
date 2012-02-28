/****************************************************************************
** Meta object code from reading C++ file 'historyitemlistmodel.h'
**
** Created: Wed Jun 24 21:01:11 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "historyitemlistmodel.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'historyitemlistmodel.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__HistoryItemListModel[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      31,   30,   30,   30, 0x05,
      53,   30,   30,   30, 0x05,

 // slots: signature, parameters, type, tag, flags
      79,   75,   30,   30, 0x0a,
     113,   75,   30,   30, 0x0a,
     149,   30,   30,   30, 0x0a,
     173,   30,   30,   30, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__HistoryItemListModel[] = {
    "MoodBox::HistoryItemListModel\0\0"
    "collectionPopulated()\0collectionItemAdded()\0"
    "key\0onCollectionItemAdded(MessageKey)\0"
    "onCollectionItemRemoved(MessageKey)\0"
    "onCollectionPopulated()\0onCollectionCleared()\0"
};

const QMetaObject MoodBox::HistoryItemListModel::staticMetaObject = {
    { &QAbstractListModel::staticMetaObject, qt_meta_stringdata_MoodBox__HistoryItemListModel,
      qt_meta_data_MoodBox__HistoryItemListModel, 0 }
};

const QMetaObject *MoodBox::HistoryItemListModel::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::HistoryItemListModel::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__HistoryItemListModel))
        return static_cast<void*>(const_cast< HistoryItemListModel*>(this));
    return QAbstractListModel::qt_metacast(_clname);
}

int MoodBox::HistoryItemListModel::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QAbstractListModel::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: collectionPopulated(); break;
        case 1: collectionItemAdded(); break;
        case 2: onCollectionItemAdded((*reinterpret_cast< const MessageKey(*)>(_a[1]))); break;
        case 3: onCollectionItemRemoved((*reinterpret_cast< const MessageKey(*)>(_a[1]))); break;
        case 4: onCollectionPopulated(); break;
        case 5: onCollectionCleared(); break;
        default: ;
        }
        _id -= 6;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::HistoryItemListModel::collectionPopulated()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void MoodBox::HistoryItemListModel::collectionItemAdded()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}
QT_END_MOC_NAMESPACE
