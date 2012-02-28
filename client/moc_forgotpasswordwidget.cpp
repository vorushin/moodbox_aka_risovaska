/****************************************************************************
** Meta object code from reading C++ file 'forgotpasswordwidget.h'
**
** Created: Wed Jun 24 21:01:11 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "forgotpasswordwidget.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'forgotpasswordwidget.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__ForgotPasswordRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      45,   32,   31,   31, 0x05,

 // slots: signature, parameters, type, tag, flags
     143,  124,   31,   31, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ForgotPasswordRequest[] = {
    "MoodBox::ForgotPasswordRequest\0\0"
    "fault,result\0"
    "forgotPasswordRequestCompleted(Fault,AccountResultCode::AccountResultC"
    "odeEnum)\0"
    "state,fault,result\0"
    "onForgotPasswordRequestResult(QVariant,Fault,AccountResultCode::Accoun"
    "tResultCodeEnum)\0"
};

const QMetaObject MoodBox::ForgotPasswordRequest::staticMetaObject = {
    { &ServerRequest::staticMetaObject, qt_meta_stringdata_MoodBox__ForgotPasswordRequest,
      qt_meta_data_MoodBox__ForgotPasswordRequest, 0 }
};

const QMetaObject *MoodBox::ForgotPasswordRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ForgotPasswordRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ForgotPasswordRequest))
        return static_cast<void*>(const_cast< ForgotPasswordRequest*>(this));
    return ServerRequest::qt_metacast(_clname);
}

int MoodBox::ForgotPasswordRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: forgotPasswordRequestCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< AccountResultCode::AccountResultCodeEnum(*)>(_a[2]))); break;
        case 1: onForgotPasswordRequestResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< AccountResultCode::AccountResultCodeEnum(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::ForgotPasswordRequest::forgotPasswordRequestCompleted(Fault _t1, AccountResultCode::AccountResultCodeEnum _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__ForgotPasswordWidget[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       8,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      31,   30,   30,   30, 0x05,
      42,   30,   30,   30, 0x05,
      49,   30,   30,   30, 0x05,
      64,   30,   30,   30, 0x05,

 // slots: signature, parameters, type, tag, flags
      91,   78,   30,   30, 0x0a,
     161,   30,   30,   30, 0x0a,
     182,   30,   30,   30, 0x08,
     212,   30,   30,   30, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__ForgotPasswordWidget[] = {
    "MoodBox::ForgotPasswordWidget\0\0"
    "finished()\0back()\0waitingStart()\0"
    "waitingStop()\0fault,result\0"
    "onResetPasswordResult(Fault,AccountResultCode::AccountResultCodeEnum)\0"
    "onRequestCancelled()\0onRestorePasswordLinkAction()\0"
    "onCancelLinkAction()\0"
};

const QMetaObject MoodBox::ForgotPasswordWidget::staticMetaObject = {
    { &ServerWidget::staticMetaObject, qt_meta_stringdata_MoodBox__ForgotPasswordWidget,
      qt_meta_data_MoodBox__ForgotPasswordWidget, 0 }
};

const QMetaObject *MoodBox::ForgotPasswordWidget::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::ForgotPasswordWidget::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__ForgotPasswordWidget))
        return static_cast<void*>(const_cast< ForgotPasswordWidget*>(this));
    if (!strcmp(_clname, "ForgotPasswordWidgetClass"))
        return static_cast< ForgotPasswordWidgetClass*>(const_cast< ForgotPasswordWidget*>(this));
    return ServerWidget::qt_metacast(_clname);
}

int MoodBox::ForgotPasswordWidget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: finished(); break;
        case 1: back(); break;
        case 2: waitingStart(); break;
        case 3: waitingStop(); break;
        case 4: onResetPasswordResult((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< AccountResultCode::AccountResultCodeEnum(*)>(_a[2]))); break;
        case 5: onRequestCancelled(); break;
        case 6: onRestorePasswordLinkAction(); break;
        case 7: onCancelLinkAction(); break;
        default: ;
        }
        _id -= 8;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::ForgotPasswordWidget::finished()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void MoodBox::ForgotPasswordWidget::back()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void MoodBox::ForgotPasswordWidget::waitingStart()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}

// SIGNAL 3
void MoodBox::ForgotPasswordWidget::waitingStop()
{
    QMetaObject::activate(this, &staticMetaObject, 3, 0);
}
QT_END_MOC_NAMESPACE
