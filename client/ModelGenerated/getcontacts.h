#ifndef GETCONTACTS_H
#define GETCONTACTS_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class GetContactsData : public QSharedData
{
public:
    GetContactsData();
    virtual ~GetContactsData();

};

class GetContacts : public TransportableObject
{
public:
    GetContacts();
    virtual ~GetContacts();

protected:
    GetContacts(GetContactsData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetContacts* ___new_()
    {
        return new GetContacts(new GetContactsData());
    }
    static GetContacts empty()
    {
        return GetContacts(new GetContactsData());
    }

    virtual bool isNull() const
    {
        return !d;
    }


    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetContactsData> d;
};

}

#endif // GETCONTACTS_H