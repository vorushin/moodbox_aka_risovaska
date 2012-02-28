#ifndef REMOVEFROMCONTACTS_H
#define REMOVEFROMCONTACTS_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class RemoveFromContactsData : public QSharedData
{
public:
    RemoveFromContactsData();
    RemoveFromContactsData(qint32 contactUserId);
    virtual ~RemoveFromContactsData();

    qint32 contactUserId;
};

class RemoveFromContacts : public TransportableObject
{
public:
    RemoveFromContacts();
    RemoveFromContacts(qint32 contactUserId);
    virtual ~RemoveFromContacts();

protected:
    RemoveFromContacts(RemoveFromContactsData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static RemoveFromContacts* ___new_()
    {
        return new RemoveFromContacts(new RemoveFromContactsData());
    }
    static RemoveFromContacts empty()
    {
        return RemoveFromContacts(new RemoveFromContactsData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getContactUserId() const;
    void setContactUserId(qint32 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<RemoveFromContactsData> d;
};

}

#endif // REMOVEFROMCONTACTS_H