/****************************************************************************
** Meta object code from reading C++ file 'publishdialog.h'
**
** Created: Wed Jun 24 21:01:17 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "publishdialog.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'publishdialog.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__PublishDialog[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      15,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // slots: signature, parameters, type, tag, flags
      24,   23,   23,   23, 0x0a,
      33,   23,   23,   23, 0x0a,
      42,   23,   23,   23, 0x09,
      78,   72,   23,   23, 0x09,
     112,   72,   23,   23, 0x09,
     156,  151,   23,   23, 0x09,
     214,  199,   23,   23, 0x09,
     267,  247,   23,   23, 0x09,
     325,  322,   23,   23, 0x09,
     369,  348,   23,   23, 0x09,
     415,  402,   23,   23, 0x09,
     449,   23,   23,   23, 0x09,
     486,  466,   23,   23, 0x09,
     516,   23,   23,   23, 0x08,
     539,   23,   23,   23, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__PublishDialog[] = {
    "MoodBox::PublishDialog\0\0accept()\0"
    "reject()\0on_publishingButton_clicked()\0"
    "state\0on_blogCheckBox_stateChanged(int)\0"
    "on_savePasswordCheck_stateChanged(int)\0"
    "text\0on_loginCombo_currentIndexChanged(QString)\0"
    "id,percentDone\0onPublishProgress(qint32,qint32)\0"
    "id,moodstripId,urls\0"
    "onPublishCompleted(qint32,qint32,QList<PublishingWay>)\0"
    "id\0onPublishError(qint32)\0"
    "success,errorMessage\0"
    "onBlogLoginChecked(bool,QString)\0"
    "success,info\0onBlogPostCompleted(bool,QString)\0"
    "publishEnabled()\0PublishingTypeIndex\0"
    "loadPublishingParameters(int)\0"
    "on_urlButton_clicked()\0"
    "on_blogUrlButton_clicked()\0"
};

const QMetaObject MoodBox::PublishDialog::staticMetaObject = {
    { &MoodBoxDialog::staticMetaObject, qt_meta_stringdata_MoodBox__PublishDialog,
      qt_meta_data_MoodBox__PublishDialog, 0 }
};

const QMetaObject *MoodBox::PublishDialog::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::PublishDialog::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__PublishDialog))
        return static_cast<void*>(const_cast< PublishDialog*>(this));
    if (!strcmp(_clname, "PublishDialogClass"))
        return static_cast< PublishDialogClass*>(const_cast< PublishDialog*>(this));
    return MoodBoxDialog::qt_metacast(_clname);
}

int MoodBox::PublishDialog::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = MoodBoxDialog::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: accept(); break;
        case 1: reject(); break;
        case 2: on_publishingButton_clicked(); break;
        case 3: on_blogCheckBox_stateChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 4: on_savePasswordCheck_stateChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 5: on_loginCombo_currentIndexChanged((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 6: onPublishProgress((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< qint32(*)>(_a[2]))); break;
        case 7: onPublishCompleted((*reinterpret_cast< qint32(*)>(_a[1])),(*reinterpret_cast< qint32(*)>(_a[2])),(*reinterpret_cast< const QList<PublishingWay>(*)>(_a[3]))); break;
        case 8: onPublishError((*reinterpret_cast< qint32(*)>(_a[1]))); break;
        case 9: onBlogLoginChecked((*reinterpret_cast< bool(*)>(_a[1])),(*reinterpret_cast< const QString(*)>(_a[2]))); break;
        case 10: onBlogPostCompleted((*reinterpret_cast< bool(*)>(_a[1])),(*reinterpret_cast< const QString(*)>(_a[2]))); break;
        case 11: publishEnabled(); break;
        case 12: loadPublishingParameters((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 13: on_urlButton_clicked(); break;
        case 14: on_blogUrlButton_clicked(); break;
        default: ;
        }
        _id -= 15;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
