#ifndef GETAUTHORIZATION_H
#define GETAUTHORIZATION_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class GetAuthorizationData : public QSharedData
{
public:
    GetAuthorizationData();
    GetAuthorizationData(qint32 userId);
    virtual ~GetAuthorizationData();

    qint32 userId;
};

class GetAuthorization : public TransportableObject
{
public:
    GetAuthorization();
    GetAuthorization(qint32 userId);
    virtual ~GetAuthorization();

protected:
    GetAuthorization(GetAuthorizationData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetAuthorization* ___new_()
    {
        return new GetAuthorization(new GetAuthorizationData());
    }
    static GetAuthorization empty()
    {
        return GetAuthorization(new GetAuthorizationData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getUserId() const;
    void setUserId(qint32 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetAuthorizationData> d;
};

}

#endif // GETAUTHORIZATION_H