#ifndef DELETEUSERFROMCHANNEL_H
#define DELETEUSERFROMCHANNEL_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class DeleteUserFromChannelData : public QSharedData
{
public:
    DeleteUserFromChannelData();
    DeleteUserFromChannelData(qint32 channelId);
    virtual ~DeleteUserFromChannelData();

    qint32 channelId;
};

class DeleteUserFromChannel : public TransportableObject
{
public:
    DeleteUserFromChannel();
    DeleteUserFromChannel(qint32 channelId);
    virtual ~DeleteUserFromChannel();

protected:
    DeleteUserFromChannel(DeleteUserFromChannelData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static DeleteUserFromChannel* ___new_()
    {
        return new DeleteUserFromChannel(new DeleteUserFromChannelData());
    }
    static DeleteUserFromChannel empty()
    {
        return DeleteUserFromChannel(new DeleteUserFromChannelData());
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
    QExplicitlySharedDataPointer<DeleteUserFromChannelData> d;
};

}

#endif // DELETEUSERFROMCHANNEL_H