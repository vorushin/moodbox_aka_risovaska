#include "listwrapperobjects.h"
#include "notificationresult.h"

namespace MoodBox
{

NotificationResultData::NotificationResultData() : QSharedData()
{
    this->packetId = 0;
}
NotificationResultData::NotificationResultData(qint64 packetId, QList<Notification> notifications) : QSharedData()
{
    this->packetId = packetId;
    this->notifications = notifications;
}

NotificationResultData::~NotificationResultData()
{
}

NotificationResult::NotificationResult() : TransportableObject()
{
}
NotificationResult::NotificationResult(qint64 packetId, QList<Notification> notifications) : TransportableObject()
{
    d = new NotificationResultData(packetId, notifications);
}

NotificationResult::~NotificationResult()
{
}

qint64 NotificationResult::getPacketId() const
{
    Q_ASSERT_X(!isNull(), "NotificationResult::getPacketId", "Getter call on object which isNull");
    return this->d->packetId;
}
void NotificationResult::setPacketId(qint64 value)
{
    Q_ASSERT_X(!isNull(), "NotificationResult::setPacketId", "Setter call on object which isNull");
    this->d->packetId = value;
}
QList<Notification> NotificationResult::getNotifications() const
{
    Q_ASSERT_X(!isNull(), "NotificationResult::getNotifications", "Getter call on object which isNull");
    return this->d->notifications;
}
void NotificationResult::setNotifications(QList<Notification> value)
{
    Q_ASSERT_X(!isNull(), "NotificationResult::setNotifications", "Setter call on object which isNull");
    this->d->notifications = value;
}

qint32 NotificationResult::getRepresentedTypeId()
{
    return 18;
}

qint32 NotificationResult::getTypeId() const
{
    return 18;
}
void NotificationResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->packetId);
    TransportableListOfSharedWrapper<Notification> notifications_wrapper(this->d->notifications);
    writer->writeProperty(this, 2, &notifications_wrapper);
}
PropertyReadResult NotificationResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->packetId = reader->readInt64();
            return PropertyReadResult(true);
        case 2:
            this->d->notifications = QList<Notification>();
            return PropertyReadResult(new ListOfSharedWrapperObject<Notification>(&this->d->notifications, PropertyInfo(true, false)));
    }

    return PropertyReadResult(false);
}

}
