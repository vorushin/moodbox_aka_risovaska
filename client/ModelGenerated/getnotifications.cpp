#include "listwrapperobjects.h"
#include "getnotifications.h"

namespace MoodBox
{

GetNotificationsData::GetNotificationsData() : QSharedData()
{
    this->packetId = 0;
}
GetNotificationsData::GetNotificationsData(QString key, qint64 packetId) : QSharedData()
{
    this->key = key;
    this->packetId = packetId;
}

GetNotificationsData::~GetNotificationsData()
{
}

GetNotifications::GetNotifications() : TransportableObject()
{
}
GetNotifications::GetNotifications(QString key, qint64 packetId) : TransportableObject()
{
    d = new GetNotificationsData(key, packetId);
}

GetNotifications::~GetNotifications()
{
}

QString GetNotifications::getKey() const
{
    Q_ASSERT_X(!isNull(), "GetNotifications::getKey", "Getter call on object which isNull");
    return this->d->key;
}
void GetNotifications::setKey(QString value)
{
    Q_ASSERT_X(!isNull(), "GetNotifications::setKey", "Setter call on object which isNull");
    this->d->key = value;
}
qint64 GetNotifications::getPacketId() const
{
    Q_ASSERT_X(!isNull(), "GetNotifications::getPacketId", "Getter call on object which isNull");
    return this->d->packetId;
}
void GetNotifications::setPacketId(qint64 value)
{
    Q_ASSERT_X(!isNull(), "GetNotifications::setPacketId", "Setter call on object which isNull");
    this->d->packetId = value;
}

qint32 GetNotifications::getRepresentedTypeId()
{
    return 10079;
}

qint32 GetNotifications::getTypeId() const
{
    return 10079;
}
void GetNotifications::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->key);
    writer->writeProperty(this, 2, this->d->packetId);
}
PropertyReadResult GetNotifications::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->key = reader->readString();
            return PropertyReadResult(true);
        case 2:
            this->d->packetId = reader->readInt64();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
