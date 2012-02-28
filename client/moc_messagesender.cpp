/****************************************************************************
** Meta object code from reading C++ file 'messagesender.h'
**
** Created: Wed Jun 24 21:01:14 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "messagesender.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'messagesender.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__MessageSender[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      57,   24,   23,   23, 0x05,
     168,  147,   23,   23, 0x05,
     251,   23,   23,   23, 0x05,
     286,  263,   23,   23, 0x05,

 // slots: signature, parameters, type, tag, flags
     369,   23,   23,   23, 0x08,
     405,  386,   23,   23, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__MessageSender[] = {
    "MoodBox::MessageSender\0\0"
    "recipientId,newKey,result,oldKey\0"
    "privateMessageSent(qint32,MessageKey,ContactResultCode::ContactResultC"
    "odeEnum,MessageKey)\0"
    "newKey,result,oldKey\0"
    "friendsMessageSent(MessageKey,ContactResultCode::ContactResultCodeEnum"
    ",MessageKey)\0"
    "sendError()\0ResultCode,recipientId\0"
    "showExceptionDialogSendingMessage(ContactResultCode::ContactResultCode"
    "Enum,qint32)\0"
    "onTimerTimeout()\0state,fault,result\0"
    "onSendMessageResult(QVariant,Fault,SendMessageResult)\0"
};

const QMetaObject MoodBox::MessageSender::staticMetaObject = {
    { &RandomRetryTimerObject::staticMetaObject, qt_meta_stringdata_MoodBox__MessageSender,
      qt_meta_data_MoodBox__MessageSender, 0 }
};

const QMetaObject *MoodBox::MessageSender::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::MessageSender::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__MessageSender))
        return static_cast<void*>(const_cast< MessageSender*>(this));
    return RandomRetryTimerObject::qt_metacast(_clname);
}

int MoodBox::MessageSender::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = RandomRetryTimerObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: privateMessageSent((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< const MessageKey(*)>(_a[2])),(*reinterpret_cast< ContactResultCode::ContactResultCodeEnum(*)>(_a[3])),(*reinterpret_cast< const MessageKey(*)>(_a[4]))); break;
        case 1: friendsMessageSent((*reinterpret_cast< const MessageKey(*)>(_a[1])),(*reinterpret_cast< ContactResultCode::ContactResultCodeEnum(*)>(_a[2])),(*reinterpret_cast< const MessageKey(*)>(_a[3]))); break;
        case 2: sendError(); break;
        case 3: showExceptionDialogSendingMessage((*reinterpret_cast< ContactResultCode::ContactResultCodeEnum(*)>(_a[1])),(*reinterpret_cast< qint32(*)>(_a[2]))); break;
        case 4: onTimerTimeout(); break;
        case 5: onSendMessageResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< SendMessageResult(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 6;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::MessageSender::privateMessageSent(qint32 _t1, const MessageKey & _t2, ContactResultCode::ContactResultCodeEnum _t3, const MessageKey & _t4)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)), const_cast<void*>(reinterpret_cast<const void*>(&_t4)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::MessageSender::friendsMessageSent(const MessageKey & _t1, ContactResultCode::ContactResultCodeEnum _t2, const MessageKey & _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void MoodBox::MessageSender::sendError()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}

// SIGNAL 3
void MoodBox::MessageSender::showExceptionDialogSendingMessage(ContactResultCode::ContactResultCodeEnum _t1, qint32 _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 3, _a);
}
QT_END_MOC_NAMESPACE
