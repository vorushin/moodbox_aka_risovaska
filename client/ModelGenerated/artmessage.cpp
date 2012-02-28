#include "listwrapperobjects.h"
#include "artmessage.h"

namespace MoodBox
{

ArtMessageData::ArtMessageData() : QSharedData()
{
    this->messageId = 0;
    this->type = MessageType::Undefined;
    this->authorId = 0;
    this->resultCode = ArtmessageResultCode::Ok;
}
ArtMessageData::ArtMessageData(qint32 messageId, MessageType::MessageTypeEnum type, qint32 authorId, QDateTime sendDate, QByteArray data, ArtmessageResultCode::ArtmessageResultCodeEnum resultCode, QString metadata, QString url) : QSharedData()
{
    this->messageId = messageId;
    this->type = type;
    this->authorId = authorId;
    this->sendDate = sendDate;
    this->data = data;
    this->resultCode = resultCode;
    this->metadata = metadata;
    this->url = url;
}

ArtMessageData::~ArtMessageData()
{
}

ArtMessage::ArtMessage() : TransportableObject()
{
}
ArtMessage::ArtMessage(qint32 messageId, MessageType::MessageTypeEnum type, qint32 authorId, QDateTime sendDate, QByteArray data, ArtmessageResultCode::ArtmessageResultCodeEnum resultCode, QString metadata, QString url) : TransportableObject()
{
    d = new ArtMessageData(messageId, type, authorId, sendDate, data, resultCode, metadata, url);
}

ArtMessage::~ArtMessage()
{
}

qint32 ArtMessage::getMessageId() const
{
    Q_ASSERT_X(!isNull(), "ArtMessage::getMessageId", "Getter call on object which isNull");
    return this->d->messageId;
}
void ArtMessage::setMessageId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "ArtMessage::setMessageId", "Setter call on object which isNull");
    this->d->messageId = value;
}
MessageType::MessageTypeEnum ArtMessage::getType() const
{
    Q_ASSERT_X(!isNull(), "ArtMessage::getType", "Getter call on object which isNull");
    return this->d->type;
}
void ArtMessage::setType(MessageType::MessageTypeEnum value)
{
    Q_ASSERT_X(!isNull(), "ArtMessage::setType", "Setter call on object which isNull");
    this->d->type = value;
}
qint32 ArtMessage::getAuthorId() const
{
    Q_ASSERT_X(!isNull(), "ArtMessage::getAuthorId", "Getter call on object which isNull");
    return this->d->authorId;
}
void ArtMessage::setAuthorId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "ArtMessage::setAuthorId", "Setter call on object which isNull");
    this->d->authorId = value;
}
QDateTime ArtMessage::getSendDate() const
{
    Q_ASSERT_X(!isNull(), "ArtMessage::getSendDate", "Getter call on object which isNull");
    return this->d->sendDate;
}
void ArtMessage::setSendDate(QDateTime value)
{
    Q_ASSERT_X(!isNull(), "ArtMessage::setSendDate", "Setter call on object which isNull");
    this->d->sendDate = value;
}
QByteArray ArtMessage::getData() const
{
    Q_ASSERT_X(!isNull(), "ArtMessage::getData", "Getter call on object which isNull");
    return this->d->data;
}
void ArtMessage::setData(QByteArray value)
{
    Q_ASSERT_X(!isNull(), "ArtMessage::setData", "Setter call on object which isNull");
    this->d->data = value;
}
ArtmessageResultCode::ArtmessageResultCodeEnum ArtMessage::getResultCode() const
{
    Q_ASSERT_X(!isNull(), "ArtMessage::getResultCode", "Getter call on object which isNull");
    return this->d->resultCode;
}
void ArtMessage::setResultCode(ArtmessageResultCode::ArtmessageResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "ArtMessage::setResultCode", "Setter call on object which isNull");
    this->d->resultCode = value;
}
QString ArtMessage::getMetadata() const
{
    Q_ASSERT_X(!isNull(), "ArtMessage::getMetadata", "Getter call on object which isNull");
    return this->d->metadata;
}
void ArtMessage::setMetadata(QString value)
{
    Q_ASSERT_X(!isNull(), "ArtMessage::setMetadata", "Setter call on object which isNull");
    this->d->metadata = value;
}
QString ArtMessage::getUrl() const
{
    Q_ASSERT_X(!isNull(), "ArtMessage::getUrl", "Getter call on object which isNull");
    return this->d->url;
}
void ArtMessage::setUrl(QString value)
{
    Q_ASSERT_X(!isNull(), "ArtMessage::setUrl", "Setter call on object which isNull");
    this->d->url = value;
}

qint32 ArtMessage::getRepresentedTypeId()
{
    return 7;
}

qint32 ArtMessage::getTypeId() const
{
    return 7;
}
void ArtMessage::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->messageId);
    writer->writeEnumProperty(this, 2, 20007, this->d->type);
    writer->writeProperty(this, 3, this->d->authorId);
    writer->writeProperty(this, 4, this->d->sendDate);
    writer->writeProperty(this, 5, this->d->data);
    writer->writeEnumProperty(this, 6, 20018, this->d->resultCode);
    writer->writeProperty(this, 7, this->d->metadata);
    writer->writeProperty(this, 8, this->d->url);
}
PropertyReadResult ArtMessage::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->messageId = reader->readInt32();
            return PropertyReadResult(true);
        case 2:
            this->d->type = (MessageType::MessageTypeEnum)reader->readEnum(20007);
            return PropertyReadResult(true);
        case 3:
            this->d->authorId = reader->readInt32();
            return PropertyReadResult(true);
        case 4:
            this->d->sendDate = reader->readDateTime();
            return PropertyReadResult(true);
        case 5:
            this->d->data = reader->readBytes();
            return PropertyReadResult(true);
        case 6:
            this->d->resultCode = (ArtmessageResultCode::ArtmessageResultCodeEnum)reader->readEnum(20018);
            return PropertyReadResult(true);
        case 7:
            this->d->metadata = reader->readString();
            return PropertyReadResult(true);
        case 8:
            this->d->url = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
