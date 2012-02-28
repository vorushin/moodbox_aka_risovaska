#include "listwrapperobjects.h"
#include "getcommands.h"

namespace MoodBox
{

GetCommandsData::GetCommandsData() : QSharedData()
{
    this->previousPackageId = 0;
}
GetCommandsData::GetCommandsData(qint32 previousPackageId) : QSharedData()
{
    this->previousPackageId = previousPackageId;
}

GetCommandsData::~GetCommandsData()
{
}

GetCommands::GetCommands() : TransportableObject()
{
}
GetCommands::GetCommands(qint32 previousPackageId) : TransportableObject()
{
    d = new GetCommandsData(previousPackageId);
}

GetCommands::~GetCommands()
{
}

qint32 GetCommands::getPreviousPackageId() const
{
    Q_ASSERT_X(!isNull(), "GetCommands::getPreviousPackageId", "Getter call on object which isNull");
    return this->d->previousPackageId;
}
void GetCommands::setPreviousPackageId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "GetCommands::setPreviousPackageId", "Setter call on object which isNull");
    this->d->previousPackageId = value;
}

qint32 GetCommands::getRepresentedTypeId()
{
    return 10049;
}

qint32 GetCommands::getTypeId() const
{
    return 10049;
}
void GetCommands::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->previousPackageId);
}
PropertyReadResult GetCommands::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->previousPackageId = reader->readInt32();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
