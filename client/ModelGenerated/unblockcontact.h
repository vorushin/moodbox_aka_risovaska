#ifndef UNBLOCKCONTACT_H
#define UNBLOCKCONTACT_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class UnblockContactData : public QSharedData
{
public:
    UnblockContactData();
    UnblockContactData(qint32 contactUserId);
    virtual ~UnblockContactData();

    qint32 contactUserId;
};

class UnblockContact : public TransportableObject
{
public:
    UnblockContact();
    UnblockContact(qint32 contactUserId);
    virtual ~UnblockContact();

protected:
    UnblockContact(UnblockContactData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static UnblockContact* ___new_()
    {
        return new UnblockContact(new UnblockContactData());
    }
    static UnblockContact empty()
    {
        return UnblockContact(new UnblockContactData());
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
    QExplicitlySharedDataPointer<UnblockContactData> d;
};

}

#endif // UNBLOCKCONTACT_H