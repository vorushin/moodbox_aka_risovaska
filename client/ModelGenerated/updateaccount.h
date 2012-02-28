#ifndef UPDATEACCOUNT_H
#define UPDATEACCOUNT_H

#include <QSharedData>
#include <QString>
#include <QByteArray>

#include "useraccount.h"
#include "transportableobject.h"

namespace MoodBox
{

class UpdateAccountData : public QSharedData
{
public:
    UpdateAccountData();
    UpdateAccountData(UserAccount userAccount, bool hasUserPicture, QByteArray userPicture, QString contentType);
    virtual ~UpdateAccountData();

    UserAccount userAccount;
    bool hasUserPicture;
    QByteArray userPicture;
    QString contentType;
};

class UpdateAccount : public TransportableObject
{
public:
    UpdateAccount();
    UpdateAccount(UserAccount userAccount, bool hasUserPicture, QByteArray userPicture, QString contentType);
    virtual ~UpdateAccount();

protected:
    UpdateAccount(UpdateAccountData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static UpdateAccount* ___new_()
    {
        return new UpdateAccount(new UpdateAccountData());
    }
    static UpdateAccount empty()
    {
        return UpdateAccount(new UpdateAccountData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    UserAccount getUserAccount() const;
    void setUserAccount(UserAccount value);
    bool getHasUserPicture() const;
    void setHasUserPicture(bool value);
    QByteArray getUserPicture() const;
    void setUserPicture(QByteArray value);
    QString getContentType() const;
    void setContentType(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<UpdateAccountData> d;
};

}

#endif // UPDATEACCOUNT_H