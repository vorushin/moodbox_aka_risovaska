/****************************************************************************
** Meta object code from reading C++ file 'tvwidget.h'
**
** Created: Wed Jun 24 21:01:20 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "tvwidget.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'tvwidget.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__ObsceneChannelMessageRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      58,   39,   38,   38, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ObsceneChannelMessageRequest[] = {
    "MoodBox::ObsceneChannelMessageRequest\0"
    "\0state,fault,result\0"
    "onObsceneChannelMessageRequestResult(QVariant,Fault,StandartResultCode"
    "::StandartResultCodeEnum)\0"
};

const QMetaObject MoodBox::ObsceneChannelMessageRequest::staticMetaObject = {
    { &ServerRequest::staticMetaObject, qt_meta_stringdata_MoodBox__ObsceneChannelMessageRequest,
      qt_meta_data_MoodBox__ObsceneChannelMessageRequest, 0 }
};

const QMetaObject *MoodBox::ObsceneChannelMessageRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ObsceneChannelMessageRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ObsceneChannelMessageRequest))
        return static_cast<void*>(const_cast< ObsceneChannelMessageRequest*>(this));
    return ServerRequest::qt_metacast(_clname);
}

int MoodBox::ObsceneChannelMessageRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onObsceneChannelMessageRequestResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< StandartResultCode::StandartResultCodeEnum(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
static const uint qt_meta_data_MoodBox__DeleteMessageRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       1,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      50,   31,   30,   30, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__DeleteMessageRequest[] = {
    "MoodBox::DeleteMessageRequest\0\0"
    "state,fault,result\0"
    "onDeleteMessageRequestResult(QVariant,Fault,StandartResultCode::Standa"
    "rtResultCodeEnum)\0"
};

const QMetaObject MoodBox::DeleteMessageRequest::staticMetaObject = {
    { &ServerRequest::staticMetaObject, qt_meta_stringdata_MoodBox__DeleteMessageRequest,
      qt_meta_data_MoodBox__DeleteMessageRequest, 0 }
};

const QMetaObject *MoodBox::DeleteMessageRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::DeleteMessageRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__DeleteMessageRequest))
        return static_cast<void*>(const_cast< DeleteMessageRequest*>(this));
    return ServerRequest::qt_metacast(_clname);
}

int MoodBox::DeleteMessageRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: onDeleteMessageRequestResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< StandartResultCode::StandartResultCodeEnum(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 1;
    }
    return _id;
}
static const uint qt_meta_data_MoodBox__TVWidget[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      39,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      25,   19,   18,   18, 0x05,
      46,   18,   18,   18, 0x05,
      67,   18,   18,   18, 0x05,
      88,   84,   18,   18, 0x05,
     123,  115,   18,   18, 0x05,
     154,  115,   18,   18, 0x05,
     181,   84,   18,   18, 0x05,
     213,   84,   18,   18, 0x05,
     247,   18,   18,   18, 0x05,
     269,   18,   18,   18, 0x05,

 // slots: signature, parameters, type, tag, flags
     289,   18,   18,   18, 0x0a,
     298,   18,   18,   18, 0x0a,
     316,   18,   18,   18, 0x0a,
     330,   84,   18,   18, 0x0a,
     358,   84,   18,   18, 0x0a,
     389,   18,   18,   18, 0x0a,
     410,   18,   18,   18, 0x0a,
     425,   84,   18,   18, 0x0a,
     452,   84,   18,   18, 0x0a,
     483,  478,   18,   18, 0x0a,
     517,   18,   18,   18, 0x0a,
     531,   18,   18,   18, 0x0a,
     545,   18,   18,   18, 0x08,
     576,  568,   18,   18, 0x08,
     629,  613,   18,   18, 0x08,
     669,   84,   18,   18, 0x08,
     735,  702,   18,   18, 0x08,
     854,  833,   18,   18, 0x08,
     958,  945,   18,   18, 0x08,
    1002,   84,   18,   18, 0x08,
    1039,   18,   18,   18, 0x08,
    1062,   18,   18,   18, 0x08,
    1081,   18,   18,   18, 0x08,
    1101,   18,   18,   18, 0x08,
    1136,   18,   18,   18, 0x08,
    1163,   18,   18,   18, 0x08,
    1196,   18,   18,   18, 0x08,
    1224,   18,   18,   18, 0x08,
    1243,   18,   18,   18, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__TVWidget[] = {
    "MoodBox::TVWidget\0\0image\0replyRequest(QImage)\0"
    "contactListRequest()\0historyRequest()\0"
    "key\0publishRequest(MessageKey)\0enabled\0"
    "previousMessageAvailable(bool)\0"
    "nextMessageAvailable(bool)\0"
    "collectionItemAdded(MessageKey)\0"
    "collectionItemRemoved(MessageKey)\0"
    "collectionPopulated()\0collectionCleared()\0"
    "reload()\0previousMessage()\0nextMessage()\0"
    "scrollToMessage(MessageKey)\0"
    "showHistoryMessage(MessageKey)\0"
    "stopShowingHistory()\0replyChanged()\0"
    "messageDeleted(MessageKey)\0"
    "deleteMessage(MessageKey)\0keys\0"
    "deleteMessages(QList<MessageKey>)\0"
    "saveMessage()\0copyMessage()\0"
    "onContactListChanged()\0id,name\0"
    "onContactNameChanged(qint32,QString)\0"
    "recipientId,key\0onPrivateMessageSent(qint32,MessageKey)\0"
    "onFriendsMessageSent(MessageKey)\0"
    "recipientId,newKey,result,oldKey\0"
    "onPrivateMessageSentUpdate(qint32,MessageKey,ContactResultCode::Contac"
    "tResultCodeEnum,MessageKey)\0"
    "newKey,result,oldKey\0"
    "onFriendsMessageSentUpdate(MessageKey,ContactResultCode::ContactResult"
    "CodeEnum,MessageKey)\0"
    "authorId,key\0onPrivateMessageReceived(qint32,MessageKey)\0"
    "onFriendsMessageReceived(MessageKey)\0"
    "onFreezeTimerExpired()\0onMouseInPreview()\0"
    "onMouseOutPreview()\0"
    "on_replyRightArrowButton_clicked()\0"
    "on_obsceneButton_clicked()\0"
    "on_deleteMessageButton_clicked()\0"
    "on_userNameButton_clicked()\0"
    "onPublishMessage()\0onDeleteMessage()\0"
};

const QMetaObject MoodBox::TVWidget::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_MoodBox__TVWidget,
      qt_meta_data_MoodBox__TVWidget, 0 }
};

const QMetaObject *MoodBox::TVWidget::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::TVWidget::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__TVWidget))
        return static_cast<void*>(const_cast< TVWidget*>(this));
    if (!strcmp(_clname, "MessageTypeMix"))
        return static_cast< MessageTypeMix*>(const_cast< TVWidget*>(this));
    if (!strcmp(_clname, "TVWidgetClass"))
        return static_cast< TVWidgetClass*>(const_cast< TVWidget*>(this));
    return QWidget::qt_metacast(_clname);
}

int MoodBox::TVWidget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: replyRequest((*reinterpret_cast< const QImage(*)>(_a[1]))); break;
        case 1: contactListRequest(); break;
        case 2: historyRequest(); break;
        case 3: publishRequest((*reinterpret_cast< const MessageKey(*)>(_a[1]))); break;
        case 4: previousMessageAvailable((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 5: nextMessageAvailable((*reinterpret_cast< bool(*)>(_a[1]))); break;
        case 6: collectionItemAdded((*reinterpret_cast< const MessageKey(*)>(_a[1]))); break;
        case 7: collectionItemRemoved((*reinterpret_cast< const MessageKey(*)>(_a[1]))); break;
        case 8: collectionPopulated(); break;
        case 9: collectionCleared(); break;
        case 10: reload(); break;
        case 11: previousMessage(); break;
        case 12: nextMessage(); break;
        case 13: scrollToMessage((*reinterpret_cast< const MessageKey(*)>(_a[1]))); break;
        case 14: showHistoryMessage((*reinterpret_cast< const MessageKey(*)>(_a[1]))); break;
        case 15: stopShowingHistory(); break;
        case 16: replyChanged(); break;
        case 17: messageDeleted((*reinterpret_cast< const MessageKey(*)>(_a[1]))); break;
        case 18: deleteMessage((*reinterpret_cast< const MessageKey(*)>(_a[1]))); break;
        case 19: deleteMessages((*reinterpret_cast< const QList<MessageKey>(*)>(_a[1]))); break;
        case 20: saveMessage(); break;
        case 21: copyMessage(); break;
        case 22: onContactListChanged(); break;
        case 23: onContactNameChanged((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const QString(*)>(_a[2]))); break;
        case 24: onPrivateMessageSent((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const MessageKey(*)>(_a[2]))); break;
        case 25: onFriendsMessageSent((*reinterpret_cast< const MessageKey(*)>(_a[1]))); break;
        case 26: onPrivateMessageSentUpdate((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const MessageKey(*)>(_a[2])),(*reinterpret_cast< ContactResultCode::ContactResultCodeEnum(*)>(_a[3])),(*reinterpret_cast< const MessageKey(*)>(_a[4]))); break;
        case 27: onFriendsMessageSentUpdate((*reinterpret_cast< const MessageKey(*)>(_a[1])),(*reinterpret_cast< ContactResultCode::ContactResultCodeEnum(*)>(_a[2])),(*reinterpret_cast< const MessageKey(*)>(_a[3]))); break;
        case 28: onPrivateMessageReceived((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const MessageKey(*)>(_a[2]))); break;
        case 29: onFriendsMessageReceived((*reinterpret_cast< const MessageKey(*)>(_a[1]))); break;
        case 30: onFreezeTimerExpired(); break;
        case 31: onMouseInPreview(); break;
        case 32: onMouseOutPreview(); break;
        case 33: on_replyRightArrowButton_clicked(); break;
        case 34: on_obsceneButton_clicked(); break;
        case 35: on_deleteMessageButton_clicked(); break;
        case 36: on_userNameButton_clicked(); break;
        case 37: onPublishMessage(); break;
        case 38: onDeleteMessage(); break;
        default: ;
        }
        _id -= 39;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::TVWidget::replyRequest(const QImage & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::TVWidget::contactListRequest()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void MoodBox::TVWidget::historyRequest()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}

// SIGNAL 3
void MoodBox::TVWidget::publishRequest(const MessageKey & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}

// SIGNAL 4
void MoodBox::TVWidget::previousMessageAvailable(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 4, _a);
}

// SIGNAL 5
void MoodBox::TVWidget::nextMessageAvailable(bool _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 5, _a);
}

// SIGNAL 6
void MoodBox::TVWidget::collectionItemAdded(const MessageKey & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 6, _a);
}

// SIGNAL 7
void MoodBox::TVWidget::collectionItemRemoved(const MessageKey & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 7, _a);
}

// SIGNAL 8
void MoodBox::TVWidget::collectionPopulated()
{
    QMetaObject::activate(this, &staticMetaObject, 8, 0);
}

// SIGNAL 9
void MoodBox::TVWidget::collectionCleared()
{
    QMetaObject::activate(this, &staticMetaObject, 9, 0);
}
QT_END_MOC_NAMESPACE
