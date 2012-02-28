#ifndef CONTACTLOGINSRESULT_H
#define CONTACTLOGINSRESULT_H

#include <QSharedData>
#include <QList>

#include "transportableobject.h"
#include "standartresultcode.h"
#include "contactlogin.h"

namespace MoodBox
{

class ContactLoginsResultData : public QSharedData
{
public:
    ContactLoginsResultData();
    ContactLoginsResultData(StandartResultCode::StandartResultCodeEnum resultCode, QList<ContactLogin> users, QList<ContactLogin> channels);
    virtual ~ContactLoginsResultData();

    StandartResultCode::StandartResultCodeEnum resultCode;
    QList<ContactLogin> users;
    QList<ContactLogin> channels;
};

class ContactLoginsResult : public TransportableObject
{
public:
    ContactLoginsResult();
    ContactLoginsResult(StandartResultCode::StandartResultCodeEnum resultCode, QList<ContactLogin> users, QList<ContactLogin> channels);
    virtual ~ContactLoginsResult();

protected:
    ContactLoginsResult(ContactLoginsResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static ContactLoginsResult* ___new_()
    {
        return new ContactLoginsResult(new ContactLoginsResultData());
    }
    static ContactLoginsResult empty()
    {
        return ContactLoginsResult(new ContactLoginsResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    StandartResultCode::StandartResultCodeEnum getResultCode() const;
    void setResultCode(StandartResultCode::StandartResultCodeEnum value);
    QList<ContactLogin> getUsers() const;
    void setUsers(QList<ContactLogin> value);
    QList<ContactLogin> getChannels() const;
    void setChannels(QList<ContactLogin> value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<ContactLoginsResultData> d;
};

}

#endif // CONTACTLOGINSRESULT_H