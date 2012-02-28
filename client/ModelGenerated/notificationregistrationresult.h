#ifndef NOTIFICATIONREGISTRATIONRESULT_H
#define NOTIFICATIONREGISTRATIONRESULT_H

#include <QSharedData>
#include <QString>

#include "transportableobject.h"

namespace MoodBox
{

class NotificationRegistrationResultData : public QSharedData
{
public:
    NotificationRegistrationResultData();
    NotificationRegistrationResultData(QString server, QString key);
    virtual ~NotificationRegistrationResultData();

    QString server;
    QString key;
};

class NotificationRegistrationResult : public TransportableObject
{
public:
    NotificationRegistrationResult();
    NotificationRegistrationResult(QString server, QString key);
    virtual ~NotificationRegistrationResult();

protected:
    NotificationRegistrationResult(NotificationRegistrationResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static NotificationRegistrationResult* ___new_()
    {
        return new NotificationRegistrationResult(new NotificationRegistrationResultData());
    }
    static NotificationRegistrationResult empty()
    {
        return NotificationRegistrationResult(new NotificationRegistrationResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    QString getServer() const;
    void setServer(QString value);
    QString getKey() const;
    void setKey(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<NotificationRegistrationResultData> d;
};

}

#endif // NOTIFICATIONREGISTRATIONRESULT_H