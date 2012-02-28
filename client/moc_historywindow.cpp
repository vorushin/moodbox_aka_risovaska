/****************************************************************************
** Meta object code from reading C++ file 'historywindow.h'
**
** Created: Wed Jun 24 21:01:12 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "historywindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'historywindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__HistoryWindow[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      14,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      28,   24,   23,   23, 0x05,
      55,   23,   23,   23, 0x05,
      70,   24,   23,   23, 0x05,
      95,   90,   23,   23, 0x05,
     129,   23,   23,   23, 0x05,
     143,   23,   23,   23, 0x05,

 // slots: signature, parameters, type, tag, flags
     157,   24,   23,   23, 0x0a,
     190,  184,   23,   23, 0x09,
     211,   23,   23,   23, 0x09,
     238,  184,   23,   23, 0x09,
     264,   23,   23,   23, 0x09,
     291,   23,   23,   23, 0x09,
     315,   23,   23,   23, 0x09,
     339,   23,   23,   23, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__HistoryWindow[] = {
    "MoodBox::HistoryWindow\0\0key\0"
    "beginOfPreview(MessageKey)\0endOfPreview()\0"
    "clicked(MessageKey)\0keys\0"
    "deleteMessages(QList<MessageKey>)\0"
    "saveMessage()\0copyMessage()\0"
    "publishMessage(MessageKey)\0index\0"
    "onHover(QModelIndex)\0updateControlsAndActions()\0"
    "onActivation(QModelIndex)\0"
    "on_publishButton_clicked()\0"
    "onCollectionPopulated()\0onCollectionItemAdded()\0"
    "onDeleteSelection()\0"
};

const QMetaObject MoodBox::HistoryWindow::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_MoodBox__HistoryWindow,
      qt_meta_data_MoodBox__HistoryWindow, 0 }
};

const QMetaObject *MoodBox::HistoryWindow::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::HistoryWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__HistoryWindow))
        return static_cast<void*>(const_cast< HistoryWindow*>(this));
    if (!strcmp(_clname, "MessageTypeMix"))
        return static_cast< MessageTypeMix*>(const_cast< HistoryWindow*>(this));
    if (!strcmp(_clname, "HistoryWindowClass"))
        return static_cast< HistoryWindowClass*>(const_cast< HistoryWindow*>(this));
    return QWidget::qt_metacast(_clname);
}

int MoodBox::HistoryWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: beginOfPreview((*reinterpret_cast< const MessageKey(*)>(_a[1]))); break;
        case 1: endOfPreview(); break;
        case 2: clicked((*reinterpret_cast< const MessageKey(*)>(_a[1]))); break;
        case 3: deleteMessages((*reinterpret_cast< const QList<MessageKey>(*)>(_a[1]))); break;
        case 4: saveMessage(); break;
        case 5: copyMessage(); break;
        case 6: publishMessage((*reinterpret_cast< const MessageKey(*)>(_a[1]))); break;
        case 7: onHover((*reinterpret_cast< const QModelIndex(*)>(_a[1]))); break;
        case 8: updateControlsAndActions(); break;
        case 9: onActivation((*reinterpret_cast< const QModelIndex(*)>(_a[1]))); break;
        case 10: on_publishButton_clicked(); break;
        case 11: onCollectionPopulated(); break;
        case 12: onCollectionItemAdded(); break;
        case 13: onDeleteSelection(); break;
        default: ;
        }
        _id -= 14;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::HistoryWindow::beginOfPreview(const MessageKey & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::HistoryWindow::endOfPreview()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void MoodBox::HistoryWindow::clicked(const MessageKey & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void MoodBox::HistoryWindow::deleteMessages(const QList<MessageKey> & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}

// SIGNAL 4
void MoodBox::HistoryWindow::saveMessage()
{
    QMetaObject::activate(this, &staticMetaObject, 4, 0);
}

// SIGNAL 5
void MoodBox::HistoryWindow::copyMessage()
{
    QMetaObject::activate(this, &staticMetaObject, 5, 0);
}
QT_END_MOC_NAMESPACE
