/****************************************************************************
** Meta object code from reading C++ file 'logonwidget.h'
**
** Created: Wed Jun 24 21:01:13 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "logonwidget.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'logonwidget.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__LogonRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      36,   23,   22,   22, 0x05,

 // slots: signature, parameters, type, tag, flags
     101,   82,   22,   22, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__LogonRequest[] = {
    "MoodBox::LogonRequest\0\0fault,result\0"
    "logonRequestCompleted(Fault,AuthTicketResult)\0"
    "state,fault,result\0"
    "onLogonRequestResult(QVariant,Fault,AuthTicketResult)\0"
};

const QMetaObject MoodBox::LogonRequest::staticMetaObject = {
    { &ServerRequest::staticMetaObject, qt_meta_stringdata_MoodBox__LogonRequest,
      qt_meta_data_MoodBox__LogonRequest, 0 }
};

const QMetaObject *MoodBox::LogonRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::LogonRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__LogonRequest))
        return static_cast<void*>(const_cast< LogonRequest*>(this));
    return ServerRequest::qt_metacast(_clname);
}

int MoodBox::LogonRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: logonRequestCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< AuthTicketResult(*)>(_a[2]))); break;
        case 1: onLogonRequestResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< AuthTicketResult(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::LogonRequest::logonRequestCompleted(Fault _t1, AuthTicketResult _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__InviteNeedRequest[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      41,   28,   27,   27, 0x05,

 // slots: signature, parameters, type, tag, flags
     105,   86,   27,   27, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__InviteNeedRequest[] = {
    "MoodBox::InviteNeedRequest\0\0fault,result\0"
    "inviteNeedRequestCompleted(Fault,ServerInfo)\0"
    "state,fault,result\0"
    "onInviteNeedRequestResult(QVariant,Fault,ServerInfo)\0"
};

const QMetaObject MoodBox::InviteNeedRequest::staticMetaObject = {
    { &ServerRequest::staticMetaObject, qt_meta_stringdata_MoodBox__InviteNeedRequest,
      qt_meta_data_MoodBox__InviteNeedRequest, 0 }
};

const QMetaObject *MoodBox::InviteNeedRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::InviteNeedRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__InviteNeedRequest))
        return static_cast<void*>(const_cast< InviteNeedRequest*>(this));
    return ServerRequest::qt_metacast(_clname);
}

int MoodBox::InviteNeedRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: inviteNeedRequestCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< ServerInfo(*)>(_a[2]))); break;
        case 1: onInviteNeedRequestResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< ServerInfo(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::InviteNeedRequest::inviteNeedRequestCompleted(Fault _t1, ServerInfo _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__LogonWidget[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
      22,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      22,   21,   21,   21, 0x05,
      39,   21,   21,   21, 0x05,
      57,   21,   21,   21, 0x05,
      76,   21,   21,   21, 0x05,
      93,   21,   21,   21, 0x05,
     108,   21,   21,   21, 0x05,
     123,   21,   21,   21, 0x05,
     138,   21,   21,   21, 0x05,
     153,   21,   21,   21, 0x05,
     168,   21,   21,   21, 0x05,

 // slots: signature, parameters, type, tag, flags
     195,  182,   21,   21, 0x0a,
     233,  182,   21,   21, 0x0a,
     270,   21,   21,   21, 0x0a,
     291,   21,   21,   21, 0x08,
     303,   21,   21,   21, 0x08,
     328,   21,   21,   21, 0x08,
     347,   21,   21,   21, 0x08,
     366,   21,   21,   21, 0x08,
     383,   21,   21,   21, 0x08,
     414,  408,   21,   21, 0x08,
     438,   21,   21,   21, 0x08,
     477,   21,   21,   21, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__LogonWidget[] = {
    "MoodBox::LogonWidget\0\0forgotPassword()\0"
    "registerNewUser()\0inviteCodeNeeded()\0"
    "changePassword()\0showSettings()\0"
    "logonStarted()\0logonStopped()\0"
    "successLogon()\0waitingStart()\0"
    "waitingStop()\0fault,result\0"
    "onLogonResult(Fault,AuthTicketResult)\0"
    "onInviteNeedResult(Fault,ServerInfo)\0"
    "onRequestCancelled()\0callLogon()\0"
    "onForgotPasswordAction()\0onSettingsAction()\0"
    "onRegisterAction()\0onSignInAction()\0"
    "onPasswordChangeAction()\0login\0"
    "onLoginChoosen(QString)\0"
    "on_savePasswordCheck_stateChanged(int)\0"
    "on_autoLogonCheckBox_stateChanged(int)\0"
};

const QMetaObject MoodBox::LogonWidget::staticMetaObject = {
    { &ServerWidget::staticMetaObject, qt_meta_stringdata_MoodBox__LogonWidget,
      qt_meta_data_MoodBox__LogonWidget, 0 }
};

const QMetaObject *MoodBox::LogonWidget::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::LogonWidget::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__LogonWidget))
        return static_cast<void*>(const_cast< LogonWidget*>(this));
    if (!strcmp(_clname, "LogonWidgetClass"))
        return static_cast< LogonWidgetClass*>(const_cast< LogonWidget*>(this));
    return ServerWidget::qt_metacast(_clname);
}

int MoodBox::LogonWidget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: forgotPassword(); break;
        case 1: registerNewUser(); break;
        case 2: inviteCodeNeeded(); break;
        case 3: changePassword(); break;
        case 4: showSettings(); break;
        case 5: logonStarted(); break;
        case 6: logonStopped(); break;
        case 7: successLogon(); break;
        case 8: waitingStart(); break;
        case 9: waitingStop(); break;
        case 10: onLogonResult((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< AuthTicketResult(*)>(_a[2]))); break;
        case 11: onInviteNeedResult((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< ServerInfo(*)>(_a[2]))); break;
        case 12: onRequestCancelled(); break;
        case 13: callLogon(); break;
        case 14: onForgotPasswordAction(); break;
        case 15: onSettingsAction(); break;
        case 16: onRegisterAction(); break;
        case 17: onSignInAction(); break;
        case 18: onPasswordChangeAction(); break;
        case 19: onLoginChoosen((*reinterpret_cast< const QString(*)>(_a[1]))); break;
        case 20: on_savePasswordCheck_stateChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        case 21: on_autoLogonCheckBox_stateChanged((*reinterpret_cast< int(*)>(_a[1]))); break;
        default: ;
        }
        _id -= 22;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::LogonWidget::forgotPassword()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void MoodBox::LogonWidget::registerNewUser()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void MoodBox::LogonWidget::inviteCodeNeeded()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}

// SIGNAL 3
void MoodBox::LogonWidget::changePassword()
{
    QMetaObject::activate(this, &staticMetaObject, 3, 0);
}

// SIGNAL 4
void MoodBox::LogonWidget::showSettings()
{
    QMetaObject::activate(this, &staticMetaObject, 4, 0);
}

// SIGNAL 5
void MoodBox::LogonWidget::logonStarted()
{
    QMetaObject::activate(this, &staticMetaObject, 5, 0);
}

// SIGNAL 6
void MoodBox::LogonWidget::logonStopped()
{
    QMetaObject::activate(this, &staticMetaObject, 6, 0);
}

// SIGNAL 7
void MoodBox::LogonWidget::successLogon()
{
    QMetaObject::activate(this, &staticMetaObject, 7, 0);
}

// SIGNAL 8
void MoodBox::LogonWidget::waitingStart()
{
    QMetaObject::activate(this, &staticMetaObject, 8, 0);
}

// SIGNAL 9
void MoodBox::LogonWidget::waitingStop()
{
    QMetaObject::activate(this, &staticMetaObject, 9, 0);
}
QT_END_MOC_NAMESPACE
