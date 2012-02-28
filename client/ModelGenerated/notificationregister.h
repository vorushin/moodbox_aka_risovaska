#ifndef NOTIFICATIONREGISTER_H
#define NOTIFICATIONREGISTER_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class NotificationRegisterData : public QSharedData
{
public:
    NotificationRegisterData();
    virtual ~NotificationRegisterData();

};

class NotificationRegister : public TransportableObject
{
public:
    NotificationRegister();
    virtual ~NotificationRegister();

protected:
    NotificationRegister(NotificationRegisterData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static NotificationRegister* ___new_()
    {
        return new NotificationRegister(new NotificationRegisterData());
    }
    static NotificationRegister empty()
    {
        return NotificationRegister(new NotificationRegisterData());
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
    QExplicitlySharedDataPointer<NotificationRegisterData> d;
};

}

#endif // NOTIFICATIONREGISTER_H