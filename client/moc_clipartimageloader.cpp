/****************************************************************************
** Meta object code from reading C++ file 'clipartimageloader.h'
**
** Created: Tue May 5 10:34:37 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "clipartimageloader.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'clipartimageloader.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__ClipartImageLoader[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      39,   29,   28,   28, 0x05,

 // slots: signature, parameters, type, tag, flags
      68,   28,   28,   28, 0x0a,
      76,   28,   28,   28, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ClipartImageLoader[] = {
    "MoodBox::ClipartImageLoader\0\0images,id\0"
    "imagesLoaded(ItemImages,int)\0clear()\0"
    "onJobFinished()\0"
};

const QMetaObject MoodBox::ClipartImageLoader::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MoodBox__ClipartImageLoader,
      qt_meta_data_MoodBox__ClipartImageLoader, 0 }
};

const QMetaObject *MoodBox::ClipartImageLoader::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ClipartImageLoader::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ClipartImageLoader))
        return static_cast<void*>(const_cast< ClipartImageLoader*>(this));
    return QObject::qt_metacast(_clname);
}

int MoodBox::ClipartImageLoader::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: imagesLoaded((*reinterpret_cast< ItemImages(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        case 1: clear(); break;
        case 2: onJobFinished(); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::ClipartImageLoader::imagesLoaded(ItemImages _t1, int _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
