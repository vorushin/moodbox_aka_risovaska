#ifndef COMMANDPACKAGE_H
#define COMMANDPACKAGE_H

#include <QSharedData>
#include <QList>

#include "transportableobject.h"
#include "packageunion.h"

namespace MoodBox
{

class CommandPackageData : public QSharedData
{
public:
    CommandPackageData();
    CommandPackageData(qint32 packageId, QList<PackageUnion> items);
    virtual ~CommandPackageData();

    qint32 packageId;
    QList<PackageUnion> items;
};

class CommandPackage : public TransportableObject
{
public:
    CommandPackage();
    CommandPackage(qint32 packageId, QList<PackageUnion> items);
    virtual ~CommandPackage();

protected:
    CommandPackage(CommandPackageData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static CommandPackage* ___new_()
    {
        return new CommandPackage(new CommandPackageData());
    }
    static CommandPackage empty()
    {
        return CommandPackage(new CommandPackageData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getPackageId() const;
    void setPackageId(qint32 value);
    QList<PackageUnion> getItems() const;
    void setItems(QList<PackageUnion> value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<CommandPackageData> d;
};

}

#endif // COMMANDPACKAGE_H