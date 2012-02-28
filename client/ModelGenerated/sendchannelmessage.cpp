#include "listwrapperobjects.h"
#include "sendchannelmessage.h"

namespace MoodBox
{

SendChannelMessageData::SendChannelMessageData() : QSharedData()
{
    this->channelId = 0;
}
SendChannelMessageData::SendChannelMessageData(qint32 channelId, QByteArray message, QString metadata) : QSharedData()
{
    this->channelId = channelId;
    this->message = message;
    this->metadata = metadata;
}

SendChannelMessageData::~SendChannelMessageData()
{
}

SendChannelMessage::SendChannelMessage() : TransportableObject()
{
}
SendChannelMessage::SendChannelMessage(qint32 channelId, QByteArray message, QString metadata) : TransportableObject()
{
    d = new SendChannelMessageData(channelId, message, metadata);
}

SendChannelMessage::~SendChannelMessage()
{
}

qint32 SendChannelMessage::getChannelId() const
{
    Q_ASSERT_X(!isNull(), "SendChannelMessage::getChannelId", "Getter call on object which isNull");
    return this->d->channelId;
}
void SendChannelMessage::setChannelId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "SendChannelMessage::setChannelId", "Setter call on object which isNull");
    this->d->channelId = value;
}
QByteArray SendChannelMessage::getMessage() const
{
    Q_ASSERT_X(!isNull(), "SendChannelMessage::getMessage", "Getter call on object which isNull");
    return this->d->message;
}
void SendChannelMessage::setMessage(QByteArray value)
{
    Q_ASSERT_X(!isNull(), "SendChannelMessage::setMessage", "Setter call on object which isNull");
    this->d->message = value;
}
QString SendChannelMessage::getMetadata() const
{
    Q_ASSERT_X(!isNull(), "SendChannelMessage::getMetadata", "Getter call on object which isNull");
    return this->d->metadata;
}
void SendChannelMessage::setMetadata(QString value)
{
    Q_ASSERT_X(!isNull(), "SendChannelMessage::setMetadata", "Setter call on object which isNull");
    this->d->metadata = value;
}

qint32 SendChannelMessage::getRepresentedTypeId()
{
    return 10105;
}

qint32 SendChannelMessage::getTypeId() const
{
    return 10105;
}
void SendChannelMessage::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->channelId);
    writer->writeProperty(this, 2, this->d->message);
    writer->writeProperty(this, 3, this->d->metadata);
}
PropertyReadResult SendChannelMessage::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
            this->d->message = reader->readBytes();
            return PropertyReadResult(true);
        case 3:
            this->d->metadata = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
