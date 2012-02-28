/****************************************************************************
** Meta object code from reading C++ file 'blogtools.h'
**
** Created: Tue May 5 10:34:35 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "blogtools.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'blogtools.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__BlogPoster[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      34,   21,   20,   20, 0x05,

 // slots: signature, parameters, type, tag, flags
      68,   62,   20,   20, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__BlogPoster[] = {
    "MoodBox::BlogPoster\0\0success,info\0"
    "postCompleted(bool,QString)\0reply\0"
    "onNetworkReply(QNetworkReply*)\0"
};

const QMetaObject MoodBox::BlogPoster::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MoodBox__BlogPoster,
      qt_meta_data_MoodBox__BlogPoster, 0 }
};

const QMetaObject *MoodBox::BlogPoster::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::BlogPoster::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__BlogPoster))
        return static_cast<void*>(const_cast< BlogPoster*>(this));
    return QObject::qt_metacast(_clname);
}

int MoodBox::BlogPoster::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: postCompleted((*reinterpret_cast< bool(*)>(_a[1])),(*reinterpret_cast< const QString(*)>(_a[2]))); break;
        case 1: onNetworkReply((*reinterpret_cast< QNetworkReply*(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::BlogPoster::postCompleted(bool _t1, const QString & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__LJPoster[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      40,   19,   18,   18, 0x05,

 // slots: signature, parameters, type, tag, flags
      73,   67,   18,   18, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__LJPoster[] = {
    "MoodBox::LJPoster\0\0success,errorMessage\0"
    "loginChecked(bool,QString)\0reply\0"
    "onNetworkReply(QNetworkReply*)\0"
};

const QMetaObject MoodBox::LJPoster::staticMetaObject = {
    { &BlogPoster::staticMetaObject, qt_meta_stringdata_MoodBox__LJPoster,
      qt_meta_data_MoodBox__LJPoster, 0 }
};

const QMetaObject *MoodBox::LJPoster::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::LJPoster::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__LJPoster))
        return static_cast<void*>(const_cast< LJPoster*>(this));
    return BlogPoster::qt_metacast(_clname);
}

int MoodBox::LJPoster::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = BlogPoster::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: loginChecked((*reinterpret_cast< bool(*)>(_a[1])),(*reinterpret_cast< const QString(*)>(_a[2]))); break;
        case 1: onNetworkReply((*reinterpret_cast< QNetworkReply*(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::LJPoster::loginChecked(bool _t1, const QString & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__LIPoster[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      40,   19,   18,   18, 0x05,

 // slots: signature, parameters, type, tag, flags
      73,   67,   18,   18, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__LIPoster[] = {
    "MoodBox::LIPoster\0\0success,errorMessage\0"
    "loginChecked(bool,QString)\0reply\0"
    "onNetworkReply(QNetworkReply*)\0"
};

const QMetaObject MoodBox::LIPoster::staticMetaObject = {
    { &BlogPoster::staticMetaObject, qt_meta_stringdata_MoodBox__LIPoster,
      qt_meta_data_MoodBox__LIPoster, 0 }
};

const QMetaObject *MoodBox::LIPoster::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::LIPoster::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__LIPoster))
        return static_cast<void*>(const_cast< LIPoster*>(this));
    return BlogPoster::qt_metacast(_clname);
}

int MoodBox::LIPoster::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = BlogPoster::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: loginChecked((*reinterpret_cast< bool(*)>(_a[1])),(*reinterpret_cast< const QString(*)>(_a[2]))); break;
        case 1: onNetworkReply((*reinterpret_cast< QNetworkReply*(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::LIPoster::loginChecked(bool _t1, const QString & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__DiaryPoster[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      43,   22,   21,   21, 0x05,

 // slots: signature, parameters, type, tag, flags
      76,   70,   21,   21, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__DiaryPoster[] = {
    "MoodBox::DiaryPoster\0\0success,errorMessage\0"
    "loginChecked(bool,QString)\0reply\0"
    "onNetworkReply(QNetworkReply*)\0"
};

const QMetaObject MoodBox::DiaryPoster::staticMetaObject = {
    { &BlogPoster::staticMetaObject, qt_meta_stringdata_MoodBox__DiaryPoster,
      qt_meta_data_MoodBox__DiaryPoster, 0 }
};

const QMetaObject *MoodBox::DiaryPoster::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::DiaryPoster::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__DiaryPoster))
        return static_cast<void*>(const_cast< DiaryPoster*>(this));
    return BlogPoster::qt_metacast(_clname);
}

int MoodBox::DiaryPoster::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = BlogPoster::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: loginChecked((*reinterpret_cast< bool(*)>(_a[1])),(*reinterpret_cast< const QString(*)>(_a[2]))); break;
        case 1: onNetworkReply((*reinterpret_cast< QNetworkReply*(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::DiaryPoster::loginChecked(bool _t1, const QString & _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
