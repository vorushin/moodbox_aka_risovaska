/****************************************************************************
** Meta object code from reading C++ file 'messagereceiver.h'
**
** Created: Wed Jun 24 21:01:14 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "messagereceiver.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'messagereceiver.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__RevolverRecieverManager[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      34,   33,   33,   33, 0x05,

 // slots: signature, parameters, type, tag, flags
      52,   33,   33,   33, 0x09,
      69,   33,   33,   33, 0x0a,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__RevolverRecieverManager[] = {
    "MoodBox::RevolverRecieverManager\0\0"
    "channelsChanged()\0onTimerTimeout()\0"
    "onContactListChanged()\0"
};

const QMetaObject MoodBox::RevolverRecieverManager::staticMetaObject = {
    { &SingleShotTimerObject::staticMetaObject, qt_meta_stringdata_MoodBox__RevolverRecieverManager,
      qt_meta_data_MoodBox__RevolverRecieverManager, 0 }
};

const QMetaObject *MoodBox::RevolverRecieverManager::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::RevolverRecieverManager::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__RevolverRecieverManager))
        return static_cast<void*>(const_cast< RevolverRecieverManager*>(this));
    return SingleShotTimerObject::qt_metacast(_clname);
}

int MoodBox::RevolverRecieverManager::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = SingleShotTimerObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: channelsChanged(); break;
        case 1: onTimerTimeout(); break;
        case 2: onContactListChanged(); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::RevolverRecieverManager::channelsChanged()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}
static const uint qt_meta_data_MoodBox__GetNextMessageRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      40,   32,   31,   31, 0x05,
      88,   70,   31,   31, 0x05,

 // slots: signature, parameters, type, tag, flags
     133,   31,   31,   31, 0x09,
     169,  150,   31,   31, 0x08,
     222,  150,   31,   31, 0x08,
     295,  289,   31,   31, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__GetNextMessageRequest[] = {
    "MoodBox::GetNextMessageRequest\0\0message\0"
    "gotNextArtMessage(ArtMessage)\0"
    "message,channelId\0"
    "gotNextChannelMessage(ChannelMessage,qint32)\0"
    "onTimerTimeout()\0state,fault,result\0"
    "onGetNextArtMessageResult(QVariant,Fault,ArtMessage)\0"
    "onGetNextChannelMessageUrlResult(QVariant,Fault,ChannelMessageUrl)\0"
    "reply\0onGetNetworkReply(QNetworkReply*)\0"
};

const QMetaObject MoodBox::GetNextMessageRequest::staticMetaObject = {
    { &RandomRetryTimerObject::staticMetaObject, qt_meta_stringdata_MoodBox__GetNextMessageRequest,
      qt_meta_data_MoodBox__GetNextMessageRequest, 0 }
};

const QMetaObject *MoodBox::GetNextMessageRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::GetNextMessageRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__GetNextMessageRequest))
        return static_cast<void*>(const_cast< GetNextMessageRequest*>(this));
    return RandomRetryTimerObject::qt_metacast(_clname);
}

int MoodBox::GetNextMessageRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = RandomRetryTimerObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: gotNextArtMessage((*reinterpret_cast< const ArtMessage(*)>(_a[1]))); break;
        case 1: gotNextChannelMessage((*reinterpret_cast< const ChannelMessage(*)>(_a[1])),(*reinterpret_cast< qint32(*)>(_a[2]))); break;
        case 2: onTimerTimeout(); break;
        case 3: onGetNextArtMessageResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< ArtMessage(*)>(_a[3]))); break;
        case 4: onGetNextChannelMessageUrlResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< ChannelMessageUrl(*)>(_a[3]))); break;
        case 5: onGetNetworkReply((*reinterpret_cast< QNetworkReply*(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 6;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::GetNextMessageRequest::gotNextArtMessage(const ArtMessage & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::GetNextMessageRequest::gotNextChannelMessage(const ChannelMessage & _t1, qint32 _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}
static const uint qt_meta_data_MoodBox__MessageReceiver[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      34,   26,   25,   25, 0x05,

 // slots: signature, parameters, type, tag, flags
      64,   25,   25,   25, 0x08,
      95,   81,   25,   25, 0x08,
     135,   26,   25,   25, 0x08,
     184,  166,   25,   25, 0x08,
     230,   25,   25,   25, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__MessageReceiver[] = {
    "MoodBox::MessageReceiver\0\0message\0"
    "messageReceived(MessageFile*)\0"
    "onTimerTimeout()\0notifications\0"
    "onGetNotifications(QList<Notification>)\0"
    "onGetNewArtMessage(ArtMessage)\0"
    "message,channelId\0"
    "onGetNewChannelMessage(ChannelMessage,qint32)\0"
    "onChannelsChanged()\0"
};

const QMetaObject MoodBox::MessageReceiver::staticMetaObject = {
    { &RandomRetryTimerObject::staticMetaObject, qt_meta_stringdata_MoodBox__MessageReceiver,
      qt_meta_data_MoodBox__MessageReceiver, 0 }
};

const QMetaObject *MoodBox::MessageReceiver::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::MessageReceiver::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__MessageReceiver))
        return static_cast<void*>(const_cast< MessageReceiver*>(this));
    return RandomRetryTimerObject::qt_metacast(_clname);
}

int MoodBox::MessageReceiver::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = RandomRetryTimerObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: messageReceived((*reinterpret_cast< MessageFile*(*)>(_a[1]))); break;
        case 1: onTimerTimeout(); break;
        case 2: onGetNotifications((*reinterpret_cast< QList<Notification>(*)>(_a[1]))); break;
        case 3: onGetNewArtMessage((*reinterpret_cast< const ArtMessage(*)>(_a[1]))); break;
        case 4: onGetNewChannelMessage((*reinterpret_cast< const ChannelMessage(*)>(_a[1])),(*reinterpret_cast< qint32(*)>(_a[2]))); break;
        case 5: onChannelsChanged(); break;
        default: ;
        }
        _id -= 6;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::MessageReceiver::messageReceived(MessageFile * _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
