#ifndef ADDUSERTOCHANNEL_H
#define ADDUSERTOCHANNEL_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class AddUserToChannelData : public QSharedData
{
public:
    AddUserToChannelData();
    AddUserToChannelData(qint32 channelId);
    virtual ~AddUserToChannelData();

    qint32 channelId;
};

class AddUserToChannel : public TransportableObject
{
public:
    AddUserToChannel();
    AddUserToChannel(qint32 channelId);
    virtual ~AddUserToChannel();

protected:
    AddUserToChannel(AddUserToChannelData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static AddUserToChannel* ___new_()
    {
        return new AddUserToChannel(new AddUserToChannelData());
    }
    static AddUserToChannel empty()
    {
        return AddUserToChannel(new AddUserToChannelData());
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
    QExplicitlySharedDataPointer<AddUserToChannelData> d;
};

}

#endif // ADDUSERTOCHANNEL_H