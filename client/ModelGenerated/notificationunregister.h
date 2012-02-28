#ifndef NOTIFICATIONUNREGISTER_H
#define NOTIFICATIONUNREGISTER_H

#include <QSharedData>
#include <QString>

#include "transportableobject.h"

namespace MoodBox
{

class NotificationUnregisterData : public QSharedData
{
public:
    NotificationUnregisterData();
    NotificationUnregisterData(QString key);
    virtual ~NotificationUnregisterData();

    QString key;
};

class NotificationUnregister : public TransportableObject
{
public:
    NotificationUnregister();
    NotificationUnregister(QString key);
    virtual ~NotificationUnregister();

protected:
    NotificationUnregister(NotificationUnregisterData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static NotificationUnregister* ___new_()
    {
        return new NotificationUnregister(new NotificationUnregisterData());
    }
    static NotificationUnregister empty()
    {
        return NotificationUnregister(new NotificationUnregisterData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    QString getKey() const;
    void setKey(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<NotificationUnregisterData> d;
};

}

#endif // NOTIFICATIONUNREGISTER_H