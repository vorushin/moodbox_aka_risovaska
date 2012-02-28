#ifndef NOTIFICATION_H
#define NOTIFICATION_H

#include <QSharedData>

#include "event.h"
#include "transportableobject.h"

namespace MoodBox
{

class NotificationData : public QSharedData
{
public:
    NotificationData();
    NotificationData(qint32 userId, Event::EventEnum event);
    virtual ~NotificationData();

    qint32 userId;
    Event::EventEnum event;
};

class Notification : public TransportableObject
{
public:
    Notification();
    Notification(qint32 userId, Event::EventEnum event);
    virtual ~Notification();

protected:
    Notification(NotificationData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static Notification* ___new_()
    {
        return new Notification(new NotificationData());
    }
    static Notification empty()
    {
        return Notification(new NotificationData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getUserId() const;
    void setUserId(qint32 value);
    Event::EventEnum getEvent() const;
    void setEvent(Event::EventEnum value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<NotificationData> d;
};

}

#endif // NOTIFICATION_H