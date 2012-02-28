/****************************************************************************
** Meta object code from reading C++ file 'clipartcategorieslist.h'
**
** Created: Tue May 5 10:34:36 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "clipartcategorieslist.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'clipartcategorieslist.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__ClipartCategoriesList[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       3,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      38,   32,   31,   31, 0x05,

 // slots: signature, parameters, type, tag, flags
      83,   76,   31,   31, 0x09,
     127,  117,   31,   31, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ClipartCategoriesList[] = {
    "MoodBox::ClipartCategoriesList\0\0items\0"
    "clipartLoadRequest(QList<ItemImages>)\0"
    "action\0on_toolbutton_triggered(QAction*)\0"
    "images,id\0onClipartItemLoaded(ItemImages,int)\0"
};

const QMetaObject MoodBox::ClipartCategoriesList::staticMetaObject = {
    { &QWidget::staticMetaObject, qt_meta_stringdata_MoodBox__ClipartCategoriesList,
      qt_meta_data_MoodBox__ClipartCategoriesList, 0 }
};

const QMetaObject *MoodBox::ClipartCategoriesList::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ClipartCategoriesList::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ClipartCategoriesList))
        return static_cast<void*>(const_cast< ClipartCategoriesList*>(this));
    return QWidget::qt_metacast(_clname);
}

int MoodBox::ClipartCategoriesList::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: clipartLoadRequest((*reinterpret_cast< const QList<ItemImages>(*)>(_a[1]))); break;
        case 1: on_toolbutton_triggered((*reinterpret_cast< QAction*(*)>(_a[1]))); break;
        case 2: onClipartItemLoaded((*reinterpret_cast< ItemImages(*)>(_a[1])),(*reinterpret_cast< int(*)>(_a[2]))); break;
        default: ;
        }
        _id -= 3;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::ClipartCategoriesList::clipartLoadRequest(const QList<ItemImages> & _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
