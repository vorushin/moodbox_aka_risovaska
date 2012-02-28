/****************************************************************************
** Meta object code from reading C++ file 'invitecodewidget.h'
**
** Created: Wed Jun 24 21:01:13 2009
**      by: The Qt Meta Object Compiler version 61 (Qt 4.5.1)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "invitecodewidget.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'invitecodewidget.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 61
#error "This file was generated using the moc from 4.5.1. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_MoodBox__InviteCodeRequest[] = {

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
     141,  122,   27,   27, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__InviteCodeRequest[] = {
    "MoodBox::InviteCodeRequest\0\0fault,result\0"
    "inviteCodeRequestCompleted(Fault,InvitationResultCode::InvitationResul"
    "tCodeEnum)\0"
    "state,fault,result\0"
    "onInviteCodeRequestResult(QVariant,Fault,InvitationResultCode::Invitat"
    "ionResultCodeEnum)\0"
};

const QMetaObject MoodBox::InviteCodeRequest::staticMetaObject = {
    { &ServerRequest::staticMetaObject, qt_meta_stringdata_MoodBox__InviteCodeRequest,
      qt_meta_data_MoodBox__InviteCodeRequest, 0 }
};

const QMetaObject *MoodBox::InviteCodeRequest::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::InviteCodeRequest::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__InviteCodeRequest))
        return static_cast<void*>(const_cast< InviteCodeRequest*>(this));
    return ServerRequest::qt_metacast(_clname);
}

int MoodBox::InviteCodeRequest::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerRequest::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: inviteCodeRequestCompleted((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< InvitationResultCode::InvitationResultCodeEnum(*)>(_a[2]))); break;
        case 1: onInviteCodeRequestResult((*reinterpret_cast< QVariant(*)>(_a[1])),(*reinterpret_cast< Fault(*)>(_a[2])),(*reinterpret_cast< InvitationResultCode::InvitationResultCodeEnum(*)>(_a[3]))); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::InviteCodeRequest::inviteCodeRequestCompleted(Fault _t1, InvitationResultCode::InvitationResultCodeEnum _t2)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)), const_cast<void*>(reinterpret_cast<const void*>(&_t2)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
static const uint qt_meta_data_MoodBox__InviteCodeWidget[] = {

 // content:
       2,       // revision
       0,       // classname
       0,    0, // classinfo
       9,   12, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors

 // signals: signature, parameters, type, tag, flags
      27,   26,   26,   26, 0x05,
      34,   26,   26,   26, 0x05,
      41,   26,   26,   26, 0x05,
      56,   26,   26,   26, 0x05,

 // slots: signature, parameters, type, tag, flags
      83,   70,   26,   26, 0x0a,
     161,   26,   26,   26, 0x0a,
     173,   26,   26,   26, 0x0a,
     194,   26,   26,   26, 0x08,
     213,   26,   26,   26, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_MoodBox__InviteCodeWidget[] = {
    "MoodBox::InviteCodeWidget\0\0next()\0"
    "back()\0waitingStart()\0waitingStop()\0"
    "fault,result\0"
    "onCheckInvitationResult(Fault,InvitationResultCode::InvitationResultCo"
    "deEnum)\0"
    "clearData()\0onRequestCancelled()\0"
    "onNextLinkAction()\0onBackLinkAction()\0"
};

const QMetaObject MoodBox::InviteCodeWidget::staticMetaObject = {
    { &ServerWidget::staticMetaObject, qt_meta_stringdata_MoodBox__InviteCodeWidget,
      qt_meta_data_MoodBox__InviteCodeWidget, 0 }
};

const QMetaObject *MoodBox::InviteCodeWidget::metaObject() const
{
    return &staticMetaObject;
}

void *MoodBox::InviteCodeWidget::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_MoodBox__InviteCodeWidget))
        return static_cast<void*>(const_cast< InviteCodeWidget*>(this));
    if (!strcmp(_clname, "InviteCodeWidgetClass"))
        return static_cast< InviteCodeWidgetClass*>(const_cast< InviteCodeWidget*>(this));
    return ServerWidget::qt_metacast(_clname);
}

int MoodBox::InviteCodeWidget::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = ServerWidget::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: next(); break;
        case 1: back(); break;
        case 2: waitingStart(); break;
        case 3: waitingStop(); break;
        case 4: onCheckInvitationResult((*reinterpret_cast< Fault(*)>(_a[1])),(*reinterpret_cast< InvitationResultCode::InvitationResultCodeEnum(*)>(_a[2]))); break;
        case 5: clearData(); break;
        case 6: onRequestCancelled(); break;
        case 7: onNextLinkAction(); break;
        case 8: onBackLinkAction(); break;
        default: ;
        }
        _id -= 9;
    }
    return _id;
}

// SIGNAL 0
void MoodBox::InviteCodeWidget::next()
{
    QMetaObject::activate(this, &staticMetaObject, 0, 0);
}

// SIGNAL 1
void MoodBox::InviteCodeWidget::back()
{
    QMetaObject::activate(this, &staticMetaObject, 1, 0);
}

// SIGNAL 2
void MoodBox::InviteCodeWidget::waitingStart()
{
    QMetaObject::activate(this, &staticMetaObject, 2, 0);
}

// SIGNAL 3
void MoodBox::InviteCodeWidget::waitingStop()
{
    QMetaObject::activate(this, &staticMetaObject, 3, 0);
}
QT_END_MOC_NAMESPACE
