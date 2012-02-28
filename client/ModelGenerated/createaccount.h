#ifndef CREATEACCOUNT_H
#define CREATEACCOUNT_H

#include <QSharedData>
#include <QString>

#include "useraccount.h"
#include "transportableobject.h"

namespace MoodBox
{

class CreateAccountData : public QSharedData
{
public:
    CreateAccountData();
    CreateAccountData(UserAccount userAccount, QString inviteCode);
    virtual ~CreateAccountData();

    UserAccount userAccount;
    QString inviteCode;
};

class CreateAccount : public TransportableObject
{
public:
    CreateAccount();
    CreateAccount(UserAccount userAccount, QString inviteCode);
    virtual ~CreateAccount();

protected:
    CreateAccount(CreateAccountData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static CreateAccount* ___new_()
    {
        return new CreateAccount(new CreateAccountData());
    }
    static CreateAccount empty()
    {
        return CreateAccount(new CreateAccountData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    UserAccount getUserAccount() const;
    void setUserAccount(UserAccount value);
    QString getInviteCode() const;
    void setInviteCode(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<CreateAccountData> d;
};

}

#endif // CREATEACCOUNT_H