#ifndef NOTIFICATIONRESULT_H
#define NOTIFICATIONRESULT_H

#include <QSharedData>
#include <QList>

#include "transportableobject.h"
#include "notification.h"

namespace MoodBox
{

class NotificationResultData : public QSharedData
{
public:
    NotificationResultData();
    NotificationResultData(qint64 packetId, QList<Notification> notifications);
    virtual ~NotificationResultData();

    qint64 packetId;
    QList<Notification> notifications;
};

class NotificationResult : public TransportableObject
{
public:
    NotificationResult();
    NotificationResult(qint64 packetId, QList<Notification> notifications);
    virtual ~NotificationResult();

protected:
    NotificationResult(NotificationResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static NotificationResult* ___new_()
    {
        return new NotificationResult(new NotificationResultData());
    }
    static NotificationResult empty()
    {
        return NotificationResult(new NotificationResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint64 getPacketId() const;
    void setPacketId(qint64 value);
    QList<Notification> getNotifications() const;
    void setNotifications(QList<Notification> value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<NotificationResultData> d;
};

}

#endif // NOTIFICATIONRESULT_H