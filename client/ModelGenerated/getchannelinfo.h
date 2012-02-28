#ifndef GETCHANNELINFO_H
#define GETCHANNELINFO_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class GetChannelInfoData : public QSharedData
{
public:
    GetChannelInfoData();
    GetChannelInfoData(qint32 channelId);
    virtual ~GetChannelInfoData();

    qint32 channelId;
};

class GetChannelInfo : public TransportableObject
{
public:
    GetChannelInfo();
    GetChannelInfo(qint32 channelId);
    virtual ~GetChannelInfo();

protected:
    GetChannelInfo(GetChannelInfoData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetChannelInfo* ___new_()
    {
        return new GetChannelInfo(new GetChannelInfoData());
    }
    static GetChannelInfo empty()
    {
        return GetChannelInfo(new GetChannelInfoData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getChannelId() const;
    void setChannelId(qint32 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetChannelInfoData> d;
};

}

#endif // GETCHANNELINFO_H