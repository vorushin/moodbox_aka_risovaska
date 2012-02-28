#ifndef GETAUTHTICKET_H
#define GETAUTHTICKET_H

#include <QSharedData>
#include <QString>

#include "transportableobject.h"

namespace MoodBox
{

class GetAuthTicketData : public QSharedData
{
public:
    GetAuthTicketData();
    GetAuthTicketData(QString login, QString password);
    virtual ~GetAuthTicketData();

    QString login;
    QString password;
};

class GetAuthTicket : public TransportableObject
{
public:
    GetAuthTicket();
    GetAuthTicket(QString login, QString password);
    virtual ~GetAuthTicket();

protected:
    GetAuthTicket(GetAuthTicketData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetAuthTicket* ___new_()
    {
        return new GetAuthTicket(new GetAuthTicketData());
    }
    static GetAuthTicket empty()
    {
        return GetAuthTicket(new GetAuthTicketData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    QString getLogin() const;
    void setLogin(QString value);
    QString getPassword() const;
    void setPassword(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetAuthTicketData> d;
};

}

#endif // GETAUTHTICKET_H