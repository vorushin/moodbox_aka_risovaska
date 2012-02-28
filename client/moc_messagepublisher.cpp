/****************************************************************************
** Meta object code from reading C++ file 'messagepublisher.h'
**
** Created: Wed Jun 24 21:01:14 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "messagepublisher.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'messagepublisher.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__MoodstripPublisher[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       6,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      44,   29,   28,   28, 0x05,
      90,   70,   28,   28, 0x05,
     146,  143,   28,   28, 0x05,

 // slots: signature, parameters, type, tag, flags
     167,   28,   28,   28, 0x08,
     203,  184,   28,   28, 0x08,
     250,  184,   28,   28, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__MoodstripPublisher[] = {
    "MoodBox::MoodstripPublisher\0\0"
    "id,percentDone\0publishing(qint32,qint32)\0"
    "id,moodstripId,urls\0"
    "publishCompleted(qint32,qint32,QList<PublishingWay>)\0"
    "id\0publishError(qint32)\0onTimerTimeout()\0"
    "state,fault,result\0"
    "onCreateMoodstripResult(QVariant,Fault,qint32)\0"
    "onAddPictureToMoodstripResult(QVariant,Fault,PublishingMoodstripResult"
    ")\0"
};

const QMetaObject MoodBox::MoodstripPublisher::staticMetaObject = {
    { &RandomRetryTimerObject::staticMetaObject, qt_meta_stringdata_MoodBox__MoodstripPublisher,
      qt_meta_data_MoodBox__MoodstripPublisher, 0 }
};

const QMetaObject *MoodBox::MoodstripPublisher::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::MoodstripPublisher::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__MoodstripPublisher))
        return static_cast<void*>(const_cast< MoodstripPublisher*>(this));
    return RandomRetryTimerObject::qt_metacast(_clname);
}

int MoodBox::MoodstripPublisher::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = RandomRetryTimerObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: publishing((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< qint32(*)>(_a[2]))); break;
        case 1: publishCompleted((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< qint32(*)>(_a[2])),(*reinterpret_cast< const QList<PublishingWay>(*)>(_a[3]))); break;
        case 2: publishError((*reinterpret_cast< qint32(*)>(_a[1]))); break;
        case 3: onTimerTimeout(); break;
        case 4: onCreateMoodstripResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< qint32(*)>(_a[3]))); break;
        case 5: onAddPictureToMoodstripResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< PublishingMoodstripResult(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 6;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::MoodstripPublisher::publishing(qint32 _t1, qint32 _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::MoodstripPublisher::publishCompleted(qint32 _t1, qint32 _t2, const QList<PublishingWay> & _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void MoodBox::MoodstripPublisher::publishError(qint32 _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}
static const uint qt_meta_data_MoodBox__MessagePublisher[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       5,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      42,   27,   26,   26, 0x05,
      88,   68,   26,   26, 0x05,
     144,  141,   26,   26, 0x05,

 // slots: signature, parameters, type, tag, flags
     165,   68,   26,   26, 0x08,
     220,  141,   26,   26, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__MessagePublisher[] = {
    "MoodBox::MessagePublisher\0\0id,percentDone\0"
    "publishing(qint32,qint32)\0id,moodstripId,urls\0"
    "publishCompleted(qint32,qint32,QList<PublishingWay>)\0"
    "id\0publishError(qint32)\0"
    "onPublishCompleted(qint32,qint32,QList<PublishingWay>)\0"
    "onPublishError(qint32)\0"
};

const QMetaObject MoodBox::MessagePublisher::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MoodBox__MessagePublisher,
      qt_meta_data_MoodBox__MessagePublisher, 0 }
};

const QMetaObject *MoodBox::MessagePublisher::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::MessagePublisher::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__MessagePublisher))
        return static_cast<void*>(const_cast< MessagePublisher*>(this));
    return QObject::qt_metacast(_clname);
}

int MoodBox::MessagePublisher::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: publishing((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< qint32(*)>(_a[2]))); break;
        case 1: publishCompleted((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< qint32(*)>(_a[2])),(*reinterpret_cast< const QList<PublishingWay>(*)>(_a[3]))); break;
        case 2: publishError((*reinterpret_cast< qint32(*)>(_a[1]))); break;
        case 3: onPublishCompleted((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< qint32(*)>(_a[2])),(*reinterpret_cast< const QList<PublishingWay>(*)>(_a[3]))); break;
        case 4: onPublishError((*reinterpret_cast< qint32(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 5;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::MessagePublisher::publishing(qint32 _t1, qint32 _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}

// SIGNAL 1
void MoodBox::MessagePublisher::publishCompleted(qint32 _t1, qint32 _t2, const QList<PublishingWay> & _t3)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)), const_cast<void*>(reinterpret_cast<const void*>(&_t3)) };
    QMetaObject::activate(this, &staticMetaObject, 1, _a);
}

// SIGNAL 2
void MoodBox::MessagePublisher::publishError(qint32 _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 2, _a);
}
QT_END_MOC_NAMESPACE
