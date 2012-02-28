#ifndef OBSCENECHANNELMESSAGE_H
#define OBSCENECHANNELMESSAGE_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class ObsceneChannelMessageData : public QSharedData
{
public:
    ObsceneChannelMessageData();
    ObsceneChannelMessageData(qint32 channelId, qint32 messageId);
    virtual ~ObsceneChannelMessageData();

    qint32 channelId;
    qint32 messageId;
};

class ObsceneChannelMessage : public TransportableObject
{
public:
    ObsceneChannelMessage();
    ObsceneChannelMessage(qint32 channelId, qint32 messageId);
    virtual ~ObsceneChannelMessage();

protected:
    ObsceneChannelMessage(ObsceneChannelMessageData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static ObsceneChannelMessage* ___new_()
    {
        return new ObsceneChannelMessage(new ObsceneChannelMessageData());
    }
    static ObsceneChannelMessage empty()
    {
        return ObsceneChannelMessage(new ObsceneChannelMessageData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getChannelId() const;
    void setChannelId(qint32 value);
    qint32 getMessageId() const;
    void setMessageId(qint32 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<ObsceneChannelMessageData> d;
};

}

#endif // OBSCENECHANNELMESSAGE_H