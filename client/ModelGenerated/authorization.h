#ifndef AUTHORIZATION_H
#define AUTHORIZATION_H

#include <QSharedData>
#include <QString>

#include "transportableobject.h"
#include "authorizationstate.h"

namespace MoodBox
{

class AuthorizationData : public QSharedData
{
public:
    AuthorizationData();
    AuthorizationData(AuthorizationState::AuthorizationStateEnum state, QString message);
    virtual ~AuthorizationData();

    AuthorizationState::AuthorizationStateEnum state;
    QString message;
};

class Authorization : public TransportableObject
{
public:
    Authorization();
    Authorization(AuthorizationState::AuthorizationStateEnum state, QString message);
    virtual ~Authorization();

protected:
    Authorization(AuthorizationData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static Authorization* ___new_()
    {
        return new Authorization(new AuthorizationData());
    }
    static Authorization empty()
    {
        return Authorization(new AuthorizationData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    AuthorizationState::AuthorizationStateEnum getState() const;
    void setState(AuthorizationState::AuthorizationStateEnum value);
    QString getMessage() const;
    void setMessage(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<AuthorizationData> d;
};

}

#endif // AUTHORIZATION_H