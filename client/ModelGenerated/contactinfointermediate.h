#ifndef CONTACTINFOINTERMEDIATE_H
#define CONTACTINFOINTERMEDIATE_H

#include <QSharedData>
#include <QString>
#include <QDate>

#include "userstatus.h"
#include "transportableobject.h"
#include "authorizationstate.h"
#include "contacttype.h"

namespace MoodBox
{

class ContactInfoData : public QSharedData
{
public:
    ContactInfoData();
    ContactInfoData(qint32 userId, QString login, UserStatus::UserStatusEnum status, QString motto, QString name, QDate birthDay, AuthorizationState::AuthorizationStateEnum authorizationState, QString message, bool isBlocked, ContactType::ContactTypeEnum type);
    virtual ~ContactInfoData();

    qint32 userId;
    QString login;
    UserStatus::UserStatusEnum status;
    QString motto;
    QString name;
    QDate birthDay;
    AuthorizationState::AuthorizationStateEnum authorizationState;
    QString message;
    bool isBlocked;
    ContactType::ContactTypeEnum type;
};

class ContactInfoIntermediate : public TransportableObject
{
protected:
    ContactInfoIntermediate();
    ContactInfoIntermediate(qint32 userId, QString login, UserStatus::UserStatusEnum status, QString motto, QString name, QDate birthDay, AuthorizationState::AuthorizationStateEnum authorizationState, QString message, bool isBlocked, ContactType::ContactTypeEnum type);
public:
    virtual ~ContactInfoIntermediate();

protected:
    ContactInfoIntermediate(ContactInfoData* dataRef)
    {
        this->d = dataRef;
    }
public:
    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getUserId() const;
    void setUserId(qint32 value);
    QString getLogin() const;
    void setLogin(QString value);
    UserStatus::UserStatusEnum getStatus() const;
    void setStatus(UserStatus::UserStatusEnum value);
    QString getMotto() const;
    void setMotto(QString value);
    QString getName() const;
    void setName(QString value);
    QDate getBirthDay() const;
    void setBirthDay(QDate value);
    AuthorizationState::AuthorizationStateEnum getAuthorizationState() const;
    void setAuthorizationState(AuthorizationState::AuthorizationStateEnum value);
    QString getMessage() const;
    void setMessage(QString value);
    bool getIsBlocked() const;
    void setIsBlocked(bool value);
    ContactType::ContactTypeEnum getType() const;
    void setType(ContactType::ContactTypeEnum value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<ContactInfoData> d;
};

}

#endif // CONTACTINFOINTERMEDIATE_H

// template of a real class

/**************************************************

#ifndef CONTACTINFO_H
#define CONTACTINFO_H

#include "contactinfointermediate.h"

namespace MoodBox
{

class ContactInfo : public ContactInfoIntermediate
{
public:
    ContactInfo() : ContactInfoIntermediate()
    {
    }
    ContactInfo(qint32 userId, QString login, UserStatus::UserStatusEnum status, QString motto, QString name, QDate birthDay, AuthorizationState::AuthorizationStateEnum authorizationState, QString message, bool isBlocked, ContactType::ContactTypeEnum type) : ContactInfoIntermediate(userId, login, status, motto, name, birthDay, authorizationState, message, isBlocked, type)
    {
    }
    virtual ~ContactInfo()
    {
    }

protected:
    ContactInfo(ContactInfoData* dataRef) : ContactInfoIntermediate(dataRef)
    {
    }

public:
    // never use ___new_ in your code!!!
    static ContactInfo* ___new_()
    {
        return new ContactInfo(new ContactInfoData());
    }
    static ContactInfo empty()
    {
        return ContactInfo(new ContactInfoData());
    }

    // below are added methods



};

}

#endif // CONTACTINFO_H

**************************************************/
