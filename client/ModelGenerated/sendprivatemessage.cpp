#include "listwrapperobjects.h"
#include "sendprivatemessage.h"

namespace MoodBox
{

SendPrivateMessageData::SendPrivateMessageData() : QSharedData()
{
    this->recipientId = 0;
}
SendPrivateMessageData::SendPrivateMessageData(qint32 recipientId, QByteArray message, QString metadata) : QSharedData()
{
    this->recipientId = recipientId;
    this->message = message;
    this->metadata = metadata;
}

SendPrivateMessageData::~SendPrivateMessageData()
{
}

SendPrivateMessage::SendPrivateMessage() : TransportableObject()
{
}
SendPrivateMessage::SendPrivateMessage(qint32 recipientId, QByteArray message, QString metadata) : TransportableObject()
{
    d = new SendPrivateMessageData(recipientId, message, metadata);
}

SendPrivateMessage::~SendPrivateMessage()
{
}

qint32 SendPrivateMessage::getRecipientId() const
{
    Q_ASSERT_X(!isNull(), "SendPrivateMessage::getRecipientId", "Getter call on object which isNull");
    return this->d->recipientId;
}
void SendPrivateMessage::setRecipientId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "SendPrivateMessage::setRecipientId", "Setter call on object which isNull");
    this->d->recipientId = value;
}
QByteArray SendPrivateMessage::getMessage() const
{
    Q_ASSERT_X(!isNull(), "SendPrivateMessage::getMessage", "Getter call on object which isNull");
    return this->d->message;
}
void SendPrivateMessage::setMessage(QByteArray value)
{
    Q_ASSERT_X(!isNull(), "SendPrivateMessage::setMessage", "Setter call on object which isNull");
    this->d->message = value;
}
QString SendPrivateMessage::getMetadata() const
{
    Q_ASSERT_X(!isNull(), "SendPrivateMessage::getMetadata", "Getter call on object which isNull");
    return this->d->metadata;
}
void SendPrivateMessage::setMetadata(QString value)
{
    Q_ASSERT_X(!isNull(), "SendPrivateMessage::setMetadata", "Setter call on object which isNull");
    this->d->metadata = value;
}

qint32 SendPrivateMessage::getRepresentedTypeId()
{
    return 10023;
}

qint32 SendPrivateMessage::getTypeId() const
{
    return 10023;
}
void SendPrivateMessage::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->recipientId);
    writer->writeProperty(this, 2, this->d->message);
    writer->writeProperty(this, 3, this->d->metadata);
}
PropertyReadResult SendPrivateMessage::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->recipientId = reader->readInt32();
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
