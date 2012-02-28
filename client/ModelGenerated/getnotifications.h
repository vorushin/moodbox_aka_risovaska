#ifndef GETNOTIFICATIONS_H
#define GETNOTIFICATIONS_H

#include <QSharedData>
#include <QString>

#include "transportableobject.h"

namespace MoodBox
{

class GetNotificationsData : public QSharedData
{
public:
    GetNotificationsData();
    GetNotificationsData(QString key, qint64 packetId);
    virtual ~GetNotificationsData();

    QString key;
    qint64 packetId;
};

class GetNotifications : public TransportableObject
{
public:
    GetNotifications();
    GetNotifications(QString key, qint64 packetId);
    virtual ~GetNotifications();

protected:
    GetNotifications(GetNotificationsData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetNotifications* ___new_()
    {
        return new GetNotifications(new GetNotificationsData());
    }
    static GetNotifications empty()
    {
        return GetNotifications(new GetNotificationsData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    QString getKey() const;
    void setKey(QString value);
    qint64 getPacketId() const;
    void setPacketId(qint64 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetNotificationsData> d;
};

}

#endif // GETNOTIFICATIONS_H