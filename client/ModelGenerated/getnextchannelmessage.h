#ifndef GETNEXTCHANNELMESSAGE_H
#define GETNEXTCHANNELMESSAGE_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class GetNextChannelMessageData : public QSharedData
{
public:
    GetNextChannelMessageData();
    GetNextChannelMessageData(qint32 channelId, qint32 lastMessageId, bool skipMessage);
    virtual ~GetNextChannelMessageData();

    qint32 channelId;
    qint32 lastMessageId;
    bool skipMessage;
};

class GetNextChannelMessage : public TransportableObject
{
public:
    GetNextChannelMessage();
    GetNextChannelMessage(qint32 channelId, qint32 lastMessageId, bool skipMessage);
    virtual ~GetNextChannelMessage();

protected:
    GetNextChannelMessage(GetNextChannelMessageData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetNextChannelMessage* ___new_()
    {
        return new GetNextChannelMessage(new GetNextChannelMessageData());
    }
    static GetNextChannelMessage empty()
    {
        return GetNextChannelMessage(new GetNextChannelMessageData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getChannelId() const;
    void setChannelId(qint32 value);
    qint32 getLastMessageId() const;
    void setLastMessageId(qint32 value);
    bool getSkipMessage() const;
    void setSkipMessage(bool value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetNextChannelMessageData> d;
};

}

#endif // GETNEXTCHANNELMESSAGE_H