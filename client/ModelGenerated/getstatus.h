#ifndef GETSTATUS_H
#define GETSTATUS_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class GetStatusData : public QSharedData
{
public:
    GetStatusData();
    GetStatusData(qint32 userId);
    virtual ~GetStatusData();

    qint32 userId;
};

class GetStatus : public TransportableObject
{
public:
    GetStatus();
    GetStatus(qint32 userId);
    virtual ~GetStatus();

protected:
    GetStatus(GetStatusData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetStatus* ___new_()
    {
        return new GetStatus(new GetStatusData());
    }
    static GetStatus empty()
    {
        return GetStatus(new GetStatusData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getUserId() const;
    void setUserId(qint32 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetStatusData> d;
};

}

#endif // GETSTATUS_H