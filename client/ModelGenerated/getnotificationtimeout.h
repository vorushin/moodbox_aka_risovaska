#ifndef GETNOTIFICATIONTIMEOUT_H
#define GETNOTIFICATIONTIMEOUT_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class GetNotificationTimeoutData : public QSharedData
{
public:
    GetNotificationTimeoutData();
    virtual ~GetNotificationTimeoutData();

};

class GetNotificationTimeout : public TransportableObject
{
public:
    GetNotificationTimeout();
    virtual ~GetNotificationTimeout();

protected:
    GetNotificationTimeout(GetNotificationTimeoutData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetNotificationTimeout* ___new_()
    {
        return new GetNotificationTimeout(new GetNotificationTimeoutData());
    }
    static GetNotificationTimeout empty()
    {
        return GetNotificationTimeout(new GetNotificationTimeoutData());
    }

    virtual bool isNull() const
    {
        return !d;
    }


    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetNotificationTimeoutData> d;
};

}

#endif // GETNOTIFICATIONTIMEOUT_H