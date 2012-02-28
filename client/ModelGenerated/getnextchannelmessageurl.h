#ifndef GETNEXTCHANNELMESSAGEURL_H
#define GETNEXTCHANNELMESSAGEURL_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class GetNextChannelMessageUrlData : public QSharedData
{
public:
    GetNextChannelMessageUrlData();
    GetNextChannelMessageUrlData(qint32 channelId, qint32 lastMessageId, bool skipMessage);
    virtual ~GetNextChannelMessageUrlData();

    qint32 channelId;
    qint32 lastMessageId;
    bool skipMessage;
};

class GetNextChannelMessageUrl : public TransportableObject
{
public:
    GetNextChannelMessageUrl();
    GetNextChannelMessageUrl(qint32 channelId, qint32 lastMessageId, bool skipMessage);
    virtual ~GetNextChannelMessageUrl();

protected:
    GetNextChannelMessageUrl(GetNextChannelMessageUrlData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetNextChannelMessageUrl* ___new_()
    {
        return new GetNextChannelMessageUrl(new GetNextChannelMessageUrlData());
    }
    static GetNextChannelMessageUrl empty()
    {
        return GetNextChannelMessageUrl(new GetNextChannelMessageUrlData());
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
    QExplicitlySharedDataPointer<GetNextChannelMessageUrlData> d;
};

}

#endif // GETNEXTCHANNELMESSAGEURL_H