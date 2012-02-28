/****************************************************************************
** Meta object code from reading C++ file 'newcontactlistwidget2.h'
**
** Created: Wed Jun 24 21:01:15 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "newcontactlistwidget2.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'newcontactlistwidget2.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__NewContactListWidget2[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      18,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      35,   32,   31,   31, 0x05,
      68,   59,   31,   31, 0x05,
     100,   31,   31,   31, 0x05,

 // slots: signature, parameters, type, tag, flags
     120,   31,   31,   31, 0x0a,
     149,  143,   31,   31, 0x0a,
     186,   32,  174,   31, 0x0a,
     213,  211,   31,   31, 0x0a,
     301,  294,   31,   31, 0x0a,
     343,   31,   31,   31, 0x0a,
     354,   31,   31,   31, 0x0a,
     366,   31,   31,   31, 0x0a,
     388,   31,   31,   31, 0x0a,
     413,   31,   31,   31, 0x0a,
     433,   31,   31,   31, 0x0a,
     456,   31,   31,   31, 0x0a,
     490,   31,   31,   31, 0x0a,
     534,  517,   31,   31, 0x09,
     574,  143,   31,   31, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__NewContactListWidget2[] = {
    "MoodBox::NewContactListWidget2\0\0id\0"
    "contactSelected(qint32)\0id,image\0"
    "contactImageDrop(qint32,QImage)\0"
    "unreadContacts(int)\0onContactListChanged()\0"
    "index\0onActivated(QModelIndex)\0"
    "ContactInfo\0onContactChanged(qint32)\0"
    ",\0"
    "onContactAuthorizationChanged(qint32,AuthorizationState::Authorization"
    "StateEnum)\0"
    "result\0onAuthorizationDialogApproveFinished(int)\0"
    "onOnline()\0onOffline()\0onUserInfoTriggered()\0"
    "onChannelInfoTriggered()\0onDeleteTriggered()\0"
    "onAuthorizeTriggered()\0"
    "onRequestAuthorizationTriggered()\0"
    "onChannelNotifyTriggered()\0current,previous\0"
    "currentChanged(QModelIndex,QModelIndex)\0"
    "onItemHovered(QModelIndex)\0"
};

const QMetaObject MoodBox::NewContactListWidget2::staticMetaObject = {
    { &QListView::staticMetaObject, qt_meta_stringdata_MoodBox__NewContactListWidget2,
      qt_meta_data_MoodBox__NewContactListWidget2, 0 }
};

const QMetaObject *MoodBox::NewContactListWidget2::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::NewContactListWidget2::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__NewContactListWidget2))
        return static_cast<void*>(const_cast< NewContactListWidget2*>(this));
    return QListView::qt_metacast(_clname);
}

int MoodBox::NewContactListWidget2::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QListView::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: contactSelected((*reinterpret_cast< qint32(*)>(_a[1]))); break;
        case 1: contactImageDrop((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const QImage(*)>(_a[2]))); break;
        case 2: unreadContacts((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 3: onContactListChanged(); break;
        case 4: onActivated((*reinterpret_cast< const QModelIndex(*)>(_a[1]))); break;
        case 5: { ContactInfo _r = onContactChanged((*reinterpret_cast< qint32(*)>(_a[1])));
            if (_a[0]) *reinterpret_cast< ContactInfo*>(_a[0]) = _r; }  break;
        case 6: onContactAuthorizationChanged((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< AuthorizationState::AuthorizationStateEnum(*)>(_a[2]))); break;
        case 7: onAuthorizationDialogApproveFinished((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 8: onOnline(); break;
        case 9: onOffline(); break;
        case 10: onUserInfoTriggered(); break;
        case 11: onChannelInfoTriggered(); break;
        case 12: onDeleteTriggered(); break;
        case 13: onAuthorizeTriggered(); break;
        case 14: onRequestAuthorizationTriggered(); break;
        case 15: onChannelNotifyTriggered(); break;
        case 16: currentChanged((*reinterpret_cast< const QModelIndex(*)>(_a[1])),(*reinterpret_cast< const QModelIndex(*)>(_a[2]))); break;
        case 17: onItemHovered((*reinterpret_cast< const QModelIndex(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 18;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::NewContactListWidget2::contactSelected(qint32 _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::NewContactListWidget2::contactImageDrop(qint32 _t1, const QImage & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void MoodBox::NewContactListWidget2::unreadContacts(int _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}
static const uint qt_meta_data_MoodBox__ContactListItemDelegate[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       0,    0, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ContactListItemDelegate[] = {
    "MoodBox::ContactListItemDelegate\0"
};

const QMetaObject MoodBox::ContactListItemDelegate::staticMetaObject = {
    { &QItemDelegate::staticMetaObject, qt_meta_stringdata_MoodBox__ContactListItemDelegate,
      qt_meta_data_MoodBox__ContactListItemDelegate, 0 }
};

const QMetaObject *MoodBox::ContactListItemDelegate::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ContactListItemDelegate::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ContactListItemDelegate))
        return static_cast<void*>(const_cast< ContactListItemDelegate*>(this));
    return QItemDelegate::qt_metacast(_clname);
}

int MoodBox::ContactListItemDelegate::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QItemDelegate::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
static const uint qt_meta_data_MoodBox__ContactListModel[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       0,    0, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ContactListModel[] = {
    "MoodBox::ContactListModel\0"
};

const QMetaObject MoodBox::ContactListModel::staticMetaObject = {
    { &QStandardItemModel::staticMetaObject, qt_meta_stringdata_MoodBox__ContactListModel,
      qt_meta_data_MoodBox__ContactListModel, 0 }
};

const QMetaObject *MoodBox::ContactListModel::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ContactListModel::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ContactListModel))
        return static_cast<void*>(const_cast< ContactListModel*>(this));
    return QStandardItemModel::qt_metacast(_clname);
}

int MoodBox::ContactListModel::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QStandardItemModel::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    return _id;
}
QT_END_MOC_NAMESPACE
