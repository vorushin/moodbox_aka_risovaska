#include "listwrapperobjects.h"
#include "getserverinfo.h"

namespace MoodBox
{

GetServerInfoData::GetServerInfoData() : QSharedData()
{
}

GetServerInfoData::~GetServerInfoData()
{
}

GetServerInfo::GetServerInfo() : TransportableObject()
{
}

GetServerInfo::~GetServerInfo()
{
}


qint32 GetServerInfo::getRepresentedTypeId()
{
    return 10093;
}

qint32 GetServerInfo::getTypeId() const
{
    return 10093;
}
void GetServerInfo::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

}
PropertyReadResult GetServerInfo::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    return PropertyReadResult(false);
}

}
