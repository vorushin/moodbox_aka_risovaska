#include "listwrapperobjects.h"
#include "notification.h"

namespace MoodBox
{

NotificationData::NotificationData() : QSharedData()
{
    this->userId = 0;
    this->event = Event::StatusChanged;
}
NotificationData::NotificationData(qint32 userId, Event::EventEnum event) : QSharedData()
{
    this->userId = userId;
    this->event = event;
}

NotificationData::~NotificationData()
{
}

Notification::Notification() : TransportableObject()
{
}
Notification::Notification(qint32 userId, Event::EventEnum event) : TransportableObject()
{
    d = new NotificationData(userId, event);
}

Notification::~Notification()
{
}

qint32 Notification::getUserId() const
{
    Q_ASSERT_X(!isNull(), "Notification::getUserId", "Getter call on object which isNull");
    return this->d->userId;
}
void Notification::setUserId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "Notification::setUserId", "Setter call on object which isNull");
    this->d->userId = value;
}
Event::EventEnum Notification::getEvent() const
{
    Q_ASSERT_X(!isNull(), "Notification::getEvent", "Getter call on object which isNull");
    return this->d->event;
}
void Notification::setEvent(Event::EventEnum value)
{
    Q_ASSERT_X(!isNull(), "Notification::setEvent", "Setter call on object which isNull");
    this->d->event = value;
}

qint32 Notification::getRepresentedTypeId()
{
    return 19;
}

qint32 Notification::getTypeId() const
{
    return 19;
}
void Notification::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->userId);
    writer->writeEnumProperty(this, 2, 20016, this->d->event);
}
PropertyReadResult Notification::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->userId = reader->readInt32();
            return PropertyReadResult(true);
        case 2:
            this->d->event = (Event::EventEnum)reader->readEnum(20016);
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
