#include "listwrapperobjects.h"
#include "commandpackage.h"

namespace MoodBox
{

CommandPackageData::CommandPackageData() : QSharedData()
{
    this->packageId = 0;
}
CommandPackageData::CommandPackageData(qint32 packageId, QList<PackageUnion> items) : QSharedData()
{
    this->packageId = packageId;
    this->items = items;
}

CommandPackageData::~CommandPackageData()
{
}

CommandPackage::CommandPackage() : TransportableObject()
{
}
CommandPackage::CommandPackage(qint32 packageId, QList<PackageUnion> items) : TransportableObject()
{
    d = new CommandPackageData(packageId, items);
}

CommandPackage::~CommandPackage()
{
}

qint32 CommandPackage::getPackageId() const
{
    Q_ASSERT_X(!isNull(), "CommandPackage::getPackageId", "Getter call on object which isNull");
    return this->d->packageId;
}
void CommandPackage::setPackageId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "CommandPackage::setPackageId", "Setter call on object which isNull");
    this->d->packageId = value;
}
QList<PackageUnion> CommandPackage::getItems() const
{
    Q_ASSERT_X(!isNull(), "CommandPackage::getItems", "Getter call on object which isNull");
    return this->d->items;
}
void CommandPackage::setItems(QList<PackageUnion> value)
{
    Q_ASSERT_X(!isNull(), "CommandPackage::setItems", "Setter call on object which isNull");
    this->d->items = value;
}

qint32 CommandPackage::getRepresentedTypeId()
{
    return 32;
}

qint32 CommandPackage::getTypeId() const
{
    return 32;
}
void CommandPackage::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->packageId);
    TransportableListOfSharedWrapper<PackageUnion> items_wrapper(this->d->items);
    writer->writeProperty(this, 2, &items_wrapper);
}
PropertyReadResult CommandPackage::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->packageId = reader->readInt32();
            return PropertyReadResult(true);
        case 2:
            this->d->items = QList<PackageUnion>();
            return PropertyReadResult(new ListOfSharedWrapperObject<PackageUnion>(&this->d->items, PropertyInfo(true, false)));
    }

    return PropertyReadResult(false);
}

}
