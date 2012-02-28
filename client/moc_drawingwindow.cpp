/****************************************************************************
** Meta object code from reading C++ file 'drawingwindow.h'
**
** Created: Wed Jun 24 21:01:09 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "drawingwindow.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'drawingwindow.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__DrawingWindow[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      24,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      24,   23,   23,   23, 0x05,
      47,   23,   23,   23, 0x05,
      61,   23,   23,   23, 0x05,

 // slots: signature, parameters, type, tag, flags
      76,   23,   23,   23, 0x0a,
      84,   23,   23,   23, 0x0a,
      98,   92,   23,   23, 0x0a,
     115,   92,   23,   23, 0x0a,
     132,   23,   23,   23, 0x08,
     159,   23,   23,   23, 0x08,
     194,   23,   23,   23, 0x08,
     226,   23,   23,   23, 0x08,
     255,   23,   23,   23, 0x08,
     285,   23,   23,   23, 0x08,
     313,   23,   23,   23, 0x08,
     345,   23,   23,   23, 0x08,
     374,  369,   23,   23, 0x08,
     402,  369,   23,   23, 0x08,
     432,   23,   23,   23, 0x08,
     449,   23,   23,   23, 0x08,
     463,   23,   23,   23, 0x08,
     485,   23,   23,   23, 0x08,
     501,   23,   23,   23, 0x08,
     527,  524,   23,   23, 0x08,
     565,   23,   23,   23, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__DrawingWindow[] = {
    "MoodBox::DrawingWindow\0\0clipartWindowRequest()\0"
    "messageSent()\0replyChanged()\0clear()\0"
    "reset()\0image\0addImage(QImage)\0"
    "addReply(QImage)\0on_penToolButton_clicked()\0"
    "on_simplebrushToolButton_clicked()\0"
    "on_oilbrushToolButton_clicked()\0"
    "on_sprayToolButton_clicked()\0"
    "on_eraserToolButton_clicked()\0"
    "on_textToolButton_clicked()\0"
    "on_picturesToolButton_clicked()\0"
    "on_sendButton_clicked()\0item\0"
    "onItemAdded(QGraphicsItem*)\0"
    "onItemRemoved(QGraphicsItem*)\0"
    "onSceneCleared()\0onSaveScene()\0"
    "onSceneColorChanged()\0onUndoChanged()\0"
    "onContactListChanged()\0id\0"
    "onContactAuthorizationChanged(qint32)\0"
    "onUserOnlineChanged()\0"
};

const QMetaObject MoodBox::DrawingWindow::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_MoodBox__DrawingWindow,
      qt_meta_data_MoodBox__DrawingWindow, 0 }
};

const QMetaObject *MoodBox::DrawingWindow::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::DrawingWindow::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__DrawingWindow))
        return static_cast<void*>(const_cast< DrawingWindow*>(this));
    if (!strcmp(_clname, "MessageTypeMix"))
        return static_cast< MessageTypeMix*>(const_cast< DrawingWindow*>(this));
    if (!strcmp(_clname, "DrawingWindowClass"))
        return static_cast< DrawingWindowClass*>(const_cast< DrawingWindow*>(this));
    return QWidget::qt_metacast(_clname);
}

int MoodBox::DrawingWindow::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: clipartWindowRequest(); break;
        case 1: messageSent(); break;
        case 2: replyChanged(); break;
        case 3: clear(); break;
        case 4: reset(); break;
        case 5: addImage((*reinterpret_cast< const QImage(*)>(_a[1]))); break;
        case 6: addReply((*reinterpret_cast< const QImage(*)>(_a[1]))); break;
        case 7: on_penToolButton_clicked(); break;
        case 8: on_simplebrushToolButton_clicked(); break;
        case 9: on_oilbrushToolButton_clicked(); break;
        case 10: on_sprayToolButton_clicked(); break;
        case 11: on_eraserToolButton_clicked(); break;
        case 12: on_textToolButton_clicked(); break;
        case 13: on_picturesToolButton_clicked(); break;
        case 14: on_sendButton_clicked(); break;
        case 15: onItemAdded((*reinterpret_cast< QGraphicsItem*(*)>(_a[1]))); break;
        case 16: onItemRemoved((*reinterpret_cast< QGraphicsItem*(*)>(_a[1]))); break;
        case 17: onSceneCleared(); break;
        case 18: onSaveScene(); break;
        case 19: onSceneColorChanged(); break;
        case 20: onUndoChanged(); break;
        case 21: onContactListChanged(); break;
        case 22: onContactAuthorizationChanged((*reinterpret_cast< qint32(*)>(_a[1]))); break;
        case 23: onUserOnlineChanged(); break;
        default: ;
        }
        _id -= 24;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::DrawingWindow::clipartWindowRequest()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void MoodBox::DrawingWindow::messageSent()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void MoodBox::DrawingWindow::replyChanged()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}
QT_END_MOC_NAMESPACE
