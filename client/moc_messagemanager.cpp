/****************************************************************************
** Meta object code from reading C++ file 'messagemanager.h'
**
** Created: Wed Jun 24 21:01:14 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "messagemanager.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'messagemanager.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__MessageManager[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      14,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      41,   25,   24,   24, 0x05,
      83,   79,   24,   24, 0x05,
     147,  114,   24,   24, 0x05,
     264,  243,   24,   24, 0x05,
     353,   24,   24,   24, 0x05,
     379,  365,   24,   24, 0x05,
     475,  462,   24,   24, 0x05,
     531,  517,   24,   24, 0x05,
     573,   79,   24,   24, 0x05,
     623,  608,   24,   24, 0x05,
     669,  649,   24,   24, 0x05,
     725,  722,   24,   24, 0x05,
     746,   79,   24,   24, 0x05,

 // slots: signature, parameters, type, tag, flags
     778,  773,   24,   24, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__MessageManager[] = {
    "MoodBox::MessageManager\0\0recipientId,key\0"
    "privateMessageSent(qint32,MessageKey)\0"
    "key\0friendsMessageSent(MessageKey)\0"
    "recipientId,newKey,result,oldKey\0"
    "privateMessageSentUpdate(qint32,MessageKey,ContactResultCode::ContactR"
    "esultCodeEnum,MessageKey)\0"
    "newKey,result,oldKey\0"
    "friendsMessageSentUpdate(MessageKey,ContactResultCode::ContactResultCo"
    "deEnum,MessageKey)\0"
    "sendError()\0ResultCode,id\0"
    "showExceptionDialogSendingMessage(ContactResultCode::ContactResultCode"
    "Enum,qint32)\0"
    "authorId,key\0privateMessageReceived(qint32,MessageKey)\0"
    "channelId,key\0channelMessageReceived(qint32,MessageKey)\0"
    "friendsMessageReceived(MessageKey)\0"
    "id,percentDone\0publishing(qint32,qint32)\0"
    "id,moodstripId,urls\0"
    "publishCompleted(qint32,qint32,QList<PublishingWay>)\0"
    "id\0publishError(qint32)\0"
    "messageDeleted(MessageKey)\0file\0"
    "onMessageReceived(MessageFile*)\0"
};

const QMetaObject MoodBox::MessageManager::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MoodBox__MessageManager,
      qt_meta_data_MoodBox__MessageManager, 0 }
};

const QMetaObject *MoodBox::MessageManager::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::MessageManager::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__MessageManager))
        return static_cast<void*>(const_cast< MessageManager*>(this));
    return QObject::qt_metacast(_clname);
}

int MoodBox::MessageManager::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: privateMessageSent((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const MessageKey(*)>(_a[2]))); break;
        case 1: friendsMessageSent((*reinterpret_cast< const MessageKey(*)>(_a[1]))); break;
        case 2: privateMessageSentUpdate((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const MessageKey(*)>(_a[2])),(*reinterpret_cast< ContactResultCode::ContactResultCodeEnum(*)>(_a[3])),(*reinterpret_cast< const MessageKey(*)>(_a[4]))); break;
        case 3: friendsMessageSentUpdate((*reinterpret_cast< const MessageKey(*)>(_a[1])),(*reinterpret_cast< ContactResultCode::ContactResultCodeEnum(*)>(_a[2])),(*reinterpret_cast< const MessageKey(*)>(_a[3]))); break;
        case 4: sendError(); break;
        case 5: showExceptionDialogSendingMessage((*reinterpret_cast< ContactResultCode::ContactResultCodeEnum(*)>(_a[1])),(*reinterpret_cast< qint32(*)>(_a[2]))); break;
        case 6: privateMessageReceived((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const MessageKey(*)>(_a[2]))); break;
        case 7: channelMessageReceived((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const MessageKey(*)>(_a[2]))); break;
        case 8: friendsMessageReceived((*reinterpret_cast< const MessageKey(*)>(_a[1]))); break;
        case 9: publishing((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< qint32(*)>(_a[2]))); break;
        case 10: publishCompleted((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< qint32(*)>(_a[2])),(*reinterpret_cast< const QList<PublishingWay>(*)>(_a[3]))); break;
        case 11: publishError((*reinterpret_cast< qint32(*)>(_a[1]))); break;
        case 12: messageDeleted((*reinterpret_cast< const MessageKey(*)>(_a[1]))); break;
        case 13: onMessageReceived((*reinterpret_cast< MessageFile*(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 14;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::MessageManager::privateMessageSent(qint32 _t1, const MessageKey & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::MessageManager::friendsMessageSent(const MessageKey & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void MoodBox::MessageManager::privateMessageSentUpdate(qint32 _t1, const MessageKey & _t2, ContactResultCode::ContactResultCodeEnum _t3, const MessageKey & _t4)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)), const_cast<void*>(reinterpret_cast<const void*>(&_t4)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}

// SIGNAL 3
void MoodBox::MessageManager::friendsMessageSentUpdate(const MessageKey & _t1, ContactResultCode::ContactResultCodeEnum _t2, const MessageKey & _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}

// SIGNAL 4
void MoodBox::MessageManager::sendError()
{
    QMetaObject::activate(this, &staticMetaObject, 4, 0);
}

// SIGNAL 5
void MoodBox::MessageManager::showExceptionDialogSendingMessage(ContactResultCode::ContactResultCodeEnum _t1, qint32 _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 5, _a);
}

// SIGNAL 6
void MoodBox::MessageManager::privateMessageReceived(qint32 _t1, const MessageKey & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 6, _a);
}

// SIGNAL 7
void MoodBox::MessageManager::channelMessageReceived(qint32 _t1, const MessageKey & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 7, _a);
}

// SIGNAL 8
void MoodBox::MessageManager::friendsMessageReceived(const MessageKey & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 8, _a);
}

// SIGNAL 9
void MoodBox::MessageManager::publishing(qint32 _t1, qint32 _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 9, _a);
}

// SIGNAL 10
void MoodBox::MessageManager::publishCompleted(qint32 _t1, qint32 _t2, const QList<PublishingWay> & _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 10, _a);
}

// SIGNAL 11
void MoodBox::MessageManager::publishError(qint32 _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 11, _a);
}

// SIGNAL 12
void MoodBox::MessageManager::messageDeleted(const MessageKey & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 12, _a);
}
QT_END_MOC_NAMESPACE
