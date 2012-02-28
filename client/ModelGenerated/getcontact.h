#ifndef GETCONTACT_H
#define GETCONTACT_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class GetContactData : public QSharedData
{
public:
    GetContactData();
    GetContactData(qint32 userId);
    virtual ~GetContactData();

    qint32 userId;
};

class GetContact : public TransportableObject
{
public:
    GetContact();
    GetContact(qint32 userId);
    virtual ~GetContact();

protected:
    GetContact(GetContactData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetContact* ___new_()
    {
        return new GetContact(new GetContactData());
    }
    static GetContact empty()
    {
        return GetContact(new GetContactData());
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
    QExplicitlySharedDataPointer<GetContactData> d;
};

}

#endif // GETCONTACT_H