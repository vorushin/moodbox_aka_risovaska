#include "listwrapperobjects.h"
#include "notificationregister.h"

namespace MoodBox
{

NotificationRegisterData::NotificationRegisterData() : QSharedData()
{
}

NotificationRegisterData::~NotificationRegisterData()
{
}

NotificationRegister::NotificationRegister() : TransportableObject()
{
}

NotificationRegister::~NotificationRegister()
{
}


qint32 NotificationRegister::getRepresentedTypeId()
{
    return 10075;
}

qint32 NotificationRegister::getTypeId() const
{
    return 10075;
}
void NotificationRegister::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

}
PropertyReadResult NotificationRegister::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    return PropertyReadResult(false);
}

}
