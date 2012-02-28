#include "listwrapperobjects.h"
#include "getnotificationtimeout.h"

namespace MoodBox
{

GetNotificationTimeoutData::GetNotificationTimeoutData() : QSharedData()
{
}

GetNotificationTimeoutData::~GetNotificationTimeoutData()
{
}

GetNotificationTimeout::GetNotificationTimeout() : TransportableObject()
{
}

GetNotificationTimeout::~GetNotificationTimeout()
{
}


qint32 GetNotificationTimeout::getRepresentedTypeId()
{
    return 10089;
}

qint32 GetNotificationTimeout::getTypeId() const
{
    return 10089;
}
void GetNotificationTimeout::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

}
PropertyReadResult GetNotificationTimeout::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    return PropertyReadResult(false);
}

}
