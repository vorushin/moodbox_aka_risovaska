#ifndef PACKAGEUNION_H
#define PACKAGEUNION_H

#include <QSharedData>

#include "deletemessagecommand.h"
#include "transportableobject.h"

namespace MoodBox
{

class PackageUnionData : public QSharedData
{
public:
    PackageUnionData();
    PackageUnionData(TransportableObject* value);
    virtual ~PackageUnionData();

    TransportableObject* value;
};

class PackageUnion : public TransportableObject
{
public:
    PackageUnion();
    PackageUnion(TransportableObject* value);
    virtual ~PackageUnion();

protected:
    PackageUnion(PackageUnionData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static PackageUnion* ___new_()
    {
        return new PackageUnion(new PackageUnionData());
    }
    static PackageUnion empty()
    {
        return PackageUnion(new PackageUnionData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    TransportableObject* getValue() const;
    void setValue(TransportableObject* value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<PackageUnionData> d;
};

}

#endif // PACKAGEUNION_H