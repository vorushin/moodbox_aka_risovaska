#include "listwrapperobjects.h"
#include "getnextchannelmessage.h"

namespace MoodBox
{

GetNextChannelMessageData::GetNextChannelMessageData() : QSharedData()
{
    this->channelId = 0;
    this->lastMessageId = 0;
    this->skipMessage = false;
}
GetNextChannelMessageData::GetNextChannelMessageData(qint32 channelId, qint32 lastMessageId, bool skipMessage) : QSharedData()
{
    this->channelId = channelId;
    this->lastMessageId = lastMessageId;
    this->skipMessage = skipMessage;
}

GetNextChannelMessageData::~GetNextChannelMessageData()
{
}

GetNextChannelMessage::GetNextChannelMessage() : TransportableObject()
{
}
GetNextChannelMessage::GetNextChannelMessage(qint32 channelId, qint32 lastMessageId, bool skipMessage) : TransportableObject()
{
    d = new GetNextChannelMessageData(channelId, lastMessageId, skipMessage);
}

GetNextChannelMessage::~GetNextChannelMessage()
{
}

qint32 GetNextChannelMessage::getChannelId() const
{
    Q_ASSERT_X(!isNull(), "GetNextChannelMessage::getChannelId", "Getter call on object which isNull");
    return this->d->channelId;
}
void GetNextChannelMessage::setChannelId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "GetNextChannelMessage::setChannelId", "Setter call on object which isNull");
    this->d->channelId = value;
}
qint32 GetNextChannelMessage::getLastMessageId() const
{
    Q_ASSERT_X(!isNull(), "GetNextChannelMessage::getLastMessageId", "Getter call on object which isNull");
    return this->d->lastMessageId;
}
void GetNextChannelMessage::setLastMessageId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "GetNextChannelMessage::setLastMessageId", "Setter call on object which isNull");
    this->d->lastMessageId = value;
}
bool GetNextChannelMessage::getSkipMessage() const
{
    Q_ASSERT_X(!isNull(), "GetNextChannelMessage::getSkipMessage", "Getter call on object which isNull");
    return this->d->skipMessage;
}
void GetNextChannelMessage::setSkipMessage(bool value)
{
    Q_ASSERT_X(!isNull(), "GetNextChannelMessage::setSkipMessage", "Setter call on object which isNull");
    this->d->skipMessage = value;
}

qint32 GetNextChannelMessage::getRepresentedTypeId()
{
    return 10107;
}

qint32 GetNextChannelMessage::getTypeId() const
{
    return 10107;
}
void GetNextChannelMessage::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->channelId);
    writer->writeProperty(this, 2, this->d->lastMessageId);
    writer->writeProperty(this, 3, this->d->skipMessage);
}
PropertyReadResult GetNextChannelMessage::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->channelId = reader->readInt32();
            return PropertyReadResult(true);
        case 2:
            this->d->lastMessageId = reader->readInt32();
            return PropertyReadResult(true);
        case 3:
            this->d->skipMessage = reader->readBool();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
