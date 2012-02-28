#ifndef BLOCKCONTACT_H
#define BLOCKCONTACT_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class BlockContactData : public QSharedData
{
public:
    BlockContactData();
    BlockContactData(qint32 contactUserId);
    virtual ~BlockContactData();

    qint32 contactUserId;
};

class BlockContact : public TransportableObject
{
public:
    BlockContact();
    BlockContact(qint32 contactUserId);
    virtual ~BlockContact();

protected:
    BlockContact(BlockContactData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static BlockContact* ___new_()
    {
        return new BlockContact(new BlockContactData());
    }
    static BlockContact empty()
    {
        return BlockContact(new BlockContactData());
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
    QExplicitlySharedDataPointer<BlockContactData> d;
};

}

#endif // BLOCKCONTACT_H