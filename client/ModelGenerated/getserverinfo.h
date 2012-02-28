#ifndef GETSERVERINFO_H
#define GETSERVERINFO_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class GetServerInfoData : public QSharedData
{
public:
    GetServerInfoData();
    virtual ~GetServerInfoData();

};

class GetServerInfo : public TransportableObject
{
public:
    GetServerInfo();
    virtual ~GetServerInfo();

protected:
    GetServerInfo(GetServerInfoData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetServerInfo* ___new_()
    {
        return new GetServerInfo(new GetServerInfoData());
    }
    static GetServerInfo empty()
    {
        return GetServerInfo(new GetServerInfoData());
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
    QExplicitlySharedDataPointer<GetServerInfoData> d;
};

}

#endif // GETSERVERINFO_H