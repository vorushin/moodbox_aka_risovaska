#include "listwrapperobjects.h"
#include "notificationunregister.h"

namespace MoodBox
{

NotificationUnregisterData::NotificationUnregisterData() : QSharedData()
{
}
NotificationUnregisterData::NotificationUnregisterData(QString key) : QSharedData()
{
    this->key = key;
}

NotificationUnregisterData::~NotificationUnregisterData()
{
}

NotificationUnregister::NotificationUnregister() : TransportableObject()
{
}
NotificationUnregister::NotificationUnregister(QString key) : TransportableObject()
{
    d = new NotificationUnregisterData(key);
}

NotificationUnregister::~NotificationUnregister()
{
}

QString NotificationUnregister::getKey() const
{
    Q_ASSERT_X(!isNull(), "NotificationUnregister::getKey", "Getter call on object which isNull");
    return this->d->key;
}
void NotificationUnregister::setKey(QString value)
{
    Q_ASSERT_X(!isNull(), "NotificationUnregister::setKey", "Setter call on object which isNull");
    this->d->key = value;
}

qint32 NotificationUnregister::getRepresentedTypeId()
{
    return 10077;
}

qint32 NotificationUnregister::getTypeId() const
{
    return 10077;
}
void NotificationUnregister::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->key);
}
PropertyReadResult NotificationUnregister::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->key = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
