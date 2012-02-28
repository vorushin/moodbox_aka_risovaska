#include "listwrapperobjects.h"
#include "obscenechannelmessage.h"

namespace MoodBox
{

ObsceneChannelMessageData::ObsceneChannelMessageData() : QSharedData()
{
    this->channelId = 0;
    this->messageId = 0;
}
ObsceneChannelMessageData::ObsceneChannelMessageData(qint32 channelId, qint32 messageId) : QSharedData()
{
    this->channelId = channelId;
    this->messageId = messageId;
}

ObsceneChannelMessageData::~ObsceneChannelMessageData()
{
}

ObsceneChannelMessage::ObsceneChannelMessage() : TransportableObject()
{
}
ObsceneChannelMessage::ObsceneChannelMessage(qint32 channelId, qint32 messageId) : TransportableObject()
{
    d = new ObsceneChannelMessageData(channelId, messageId);
}

ObsceneChannelMessage::~ObsceneChannelMessage()
{
}

qint32 ObsceneChannelMessage::getChannelId() const
{
    Q_ASSERT_X(!isNull(), "ObsceneChannelMessage::getChannelId", "Getter call on object which isNull");
    return this->d->channelId;
}
void ObsceneChannelMessage::setChannelId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "ObsceneChannelMessage::setChannelId", "Setter call on object which isNull");
    this->d->channelId = value;
}
qint32 ObsceneChannelMessage::getMessageId() const
{
    Q_ASSERT_X(!isNull(), "ObsceneChannelMessage::getMessageId", "Getter call on object which isNull");
    return this->d->messageId;
}
void ObsceneChannelMessage::setMessageId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "ObsceneChannelMessage::setMessageId", "Setter call on object which isNull");
    this->d->messageId = value;
}

qint32 ObsceneChannelMessage::getRepresentedTypeId()
{
    return 10025;
}

qint32 ObsceneChannelMessage::getTypeId() const
{
    return 10025;
}
void ObsceneChannelMessage::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->channelId);
    writer->writeProperty(this, 2, this->d->messageId);
}
PropertyReadResult ObsceneChannelMessage::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
            this->d->messageId = reader->readInt32();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
