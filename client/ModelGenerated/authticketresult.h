#ifndef AUTHTICKETRESULT_H
#define AUTHTICKETRESULT_H

#include <QSharedData>
#include <QByteArray>

#include "transportableobject.h"
#include "authticketresultcode.h"

namespace MoodBox
{

class AuthTicketResultData : public QSharedData
{
public:
    AuthTicketResultData();
    AuthTicketResultData(QByteArray authTicket, qint32 userId, qint32 lifetime, AuthTicketResultCode::AuthTicketResultCodeEnum resultCode, qint32 lockTime);
    virtual ~AuthTicketResultData();

    QByteArray authTicket;
    qint32 userId;
    qint32 lifetime;
    AuthTicketResultCode::AuthTicketResultCodeEnum resultCode;
    qint32 lockTime;
};

class AuthTicketResult : public TransportableObject
{
public:
    AuthTicketResult();
    AuthTicketResult(QByteArray authTicket, qint32 userId, qint32 lifetime, AuthTicketResultCode::AuthTicketResultCodeEnum resultCode, qint32 lockTime);
    virtual ~AuthTicketResult();

protected:
    AuthTicketResult(AuthTicketResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static AuthTicketResult* ___new_()
    {
        return new AuthTicketResult(new AuthTicketResultData());
    }
    static AuthTicketResult empty()
    {
        return AuthTicketResult(new AuthTicketResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    QByteArray getAuthTicket() const;
    void setAuthTicket(QByteArray value);
    qint32 getUserId() const;
    void setUserId(qint32 value);
    qint32 getLifetime() const;
    void setLifetime(qint32 value);
    AuthTicketResultCode::AuthTicketResultCodeEnum getResultCode() const;
    void setResultCode(AuthTicketResultCode::AuthTicketResultCodeEnum value);
    qint32 getLockTime() const;
    void setLockTime(qint32 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<AuthTicketResultData> d;
};

}

#endif // AUTHTICKETRESULT_H