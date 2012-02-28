/****************************************************************************
** Meta object code from reading C++ file 'autoupdater.h'
**
** Created: Wed Jun 24 21:01:06 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "autoupdater.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'autoupdater.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__AutoUpdater[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       7,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      22,   21,   21,   21, 0x05,

 // slots: signature, parameters, type, tag, flags
      43,   21,   21,   21, 0x08,
      60,   51,   21,   21, 0x08,
     108,   88,   21,   21, 0x08,
     169,   21,   21,   21, 0x08,
     191,   21,   21,   21, 0x08,
     211,  204,   21,   21, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__AutoUpdater[] = {
    "MoodBox::AutoUpdater\0\0downloadNewVersion()\0"
    "start()\0id,error\0onRequestFinished(int,bool)\0"
    "proxy,authenticator\0"
    "onProxyAuthenticationRequired(QNetworkProxy,QAuthenticator*)\0"
    "onInteractiveDialog()\0onCanceled()\0"
    "result\0onDialogFinished(int)\0"
};

const QMetaObject MoodBox::AutoUpdater::staticMetaObject = {
    { &QObject::staticMetaObject, qt_meta_stringdata_MoodBox__AutoUpdater,
      qt_meta_data_MoodBox__AutoUpdater, 0 }
};

const QMetaObject *MoodBox::AutoUpdater::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::AutoUpdater::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__AutoUpdater))
        return static_cast<void*>(const_cast< AutoUpdater*>(this));
    return QObject::qt_metacast(_clname);
}

int MoodBox::AutoUpdater::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QObject::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: downloadNewVersion(); break;
        case 1: start(); break;
        case 2: onRequestFinished((*reinterpret_cast< int(*)>(_a[1])),(*reinterpret_cast< bool(*)>(_a[2]))); break;
        case 3: onProxyAuthenticationRequired((*reinterpret_cast< const QNetworkProxy(*)>(_a[1])),(*reinterpret_cast< QAuthenticator*(*)>(_a[2]))); break;
        case 4: onInteractiveDialog(); break;
        case 5: onCanceled(); break;
        case 6: onDialogFinished((*reinterpret_cast< int(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 7;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::AutoUpdater::downloadNewVersion()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}
QT_END_MOC_NAMESPACE
