/****************************************************************************
** Meta object code from reading C++ file 'registrationwidget.h'
**
** Created: Wed Jun 24 21:01:17 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "registrationwidget.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'registrationwidget.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__RegistrationRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      43,   30,   29,   29, 0x05,

 // slots: signature, parameters, type, tag, flags
     139,  120,   29,   29, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__RegistrationRequest[] = {
    "MoodBox::RegistrationRequest\0\0"
    "fault,result\0"
    "registrationRequestCompleted(Fault,AccountResultCode::AccountResultCod"
    "eEnum)\0"
    "state,fault,result\0"
    "onGetRegistrationRequestResult(QVariant,Fault,AccountResultCode::Accou"
    "ntResultCodeEnum)\0"
};

const QMetaObject MoodBox::RegistrationRequest::staticMetaObject = {
    { &ServerRequest::staticMetaObject, qt_meta_stringdata_MoodBox__RegistrationRequest,
      qt_meta_data_MoodBox__RegistrationRequest, 0 }
};

const QMetaObject *MoodBox::RegistrationRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::RegistrationRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__RegistrationRequest))
        return static_cast<void*>(const_cast< RegistrationRequest*>(this));
    return ServerRequest::qt_metacast(_clname);
}

int MoodBox::RegistrationRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: registrationRequestCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< AccountResultCode::AccountResultCodeEnum(*)>(_a[2]))); break;
        case 1: onGetRegistrationRequestResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< AccountResultCode::AccountResultCodeEnum(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::RegistrationRequest::registrationRequestCompleted(Fault _t1, AccountResultCode::AccountResultCodeEnum _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__RegistrationWidget[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      10,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      29,   28,   28,   28, 0x05,
      36,   28,   28,   28, 0x05,
      58,   28,   28,   28, 0x05,
      73,   28,   28,   28, 0x05,

 // slots: signature, parameters, type, tag, flags
     100,   87,   28,   28, 0x0a,
     170,   28,   28,   28, 0x0a,
     191,   28,   28,   28, 0x08,
     210,   28,   28,   28, 0x08,
     229,   28,   28,   28, 0x08,
     258,   28,   28,   28, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__RegistrationWidget[] = {
    "MoodBox::RegistrationWidget\0\0back()\0"
    "registrationSuccess()\0waitingStart()\0"
    "waitingHide()\0fault,result\0"
    "onCreateAccountResult(Fault,AccountResultCode::AccountResultCodeEnum)\0"
    "onRequestCancelled()\0onNextLinkAction()\0"
    "onBackLinkAction()\0on_termOfUseButton_clicked()\0"
    "on_privacyPolicyButton_clicked()\0"
};

const QMetaObject MoodBox::RegistrationWidget::staticMetaObject = {
    { &ServerWidget::staticMetaObject, qt_meta_stringdata_MoodBox__RegistrationWidget,
      qt_meta_data_MoodBox__RegistrationWidget, 0 }
};

const QMetaObject *MoodBox::RegistrationWidget::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::RegistrationWidget::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__RegistrationWidget))
        return static_cast<void*>(const_cast< RegistrationWidget*>(this));
    if (!strcmp(_clname, "RegistrationWidgetClass"))
        return static_cast< RegistrationWidgetClass*>(const_cast< RegistrationWidget*>(this));
    return ServerWidget::qt_metacast(_clname);
}

int MoodBox::RegistrationWidget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: back(); break;
        case 1: registrationSuccess(); break;
        case 2: waitingStart(); break;
        case 3: waitingHide(); break;
        case 4: onCreateAccountResult((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< AccountResultCode::AccountResultCodeEnum(*)>(_a[2]))); break;
        case 5: onRequestCancelled(); break;
        case 6: onNextLinkAction(); break;
        case 7: onBackLinkAction(); break;
        case 8: on_termOfUseButton_clicked(); break;
        case 9: on_privacyPolicyButton_clicked(); break;
        default: ;
        }
        _id -= 10;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::RegistrationWidget::back()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void MoodBox::RegistrationWidget::registrationSuccess()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void MoodBox::RegistrationWidget::waitingStart()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}

// SIGNAL 3
void MoodBox::RegistrationWidget::waitingHide()
{
    QMetaObject::activate(this, &staticMetaObject, 3, 0);
}
QT_END_MOC_NAMESPACE
