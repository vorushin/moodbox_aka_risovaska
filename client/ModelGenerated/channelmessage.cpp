#include "listwrapperobjects.h"
#include "channelmessage.h"

namespace MoodBox
{

ChannelMessageData::ChannelMessageData() : QSharedData()
{
    this->resultCode = ArtmessageResultCode::Ok;
    this->messageId = 0;
    this->authorId = 0;
}
ChannelMessageData::ChannelMessageData(ArtmessageResultCode::ArtmessageResultCodeEnum resultCode, qint32 messageId, qint32 authorId, QString authorLogin, QDateTime sendDate, QByteArray data, QString metadata) : QSharedData()
{
    this->resultCode = resultCode;
    this->messageId = messageId;
    this->authorId = authorId;
    this->authorLogin = authorLogin;
    this->sendDate = sendDate;
    this->data = data;
    this->metadata = metadata;
}

ChannelMessageData::~ChannelMessageData()
{
}

ChannelMessage::ChannelMessage() : TransportableObject()
{
}
ChannelMessage::ChannelMessage(ArtmessageResultCode::ArtmessageResultCodeEnum resultCode, qint32 messageId, qint32 authorId, QString authorLogin, QDateTime sendDate, QByteArray data, QString metadata) : TransportableObject()
{
    d = new ChannelMessageData(resultCode, messageId, authorId, authorLogin, sendDate, data, metadata);
}

ChannelMessage::~ChannelMessage()
{
}

ArtmessageResultCode::ArtmessageResultCodeEnum ChannelMessage::getResultCode() const
{
    Q_ASSERT_X(!isNull(), "ChannelMessage::getResultCode", "Getter call on object which isNull");
    return this->d->resultCode;
}
void ChannelMessage::setResultCode(ArtmessageResultCode::ArtmessageResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "ChannelMessage::setResultCode", "Setter call on object which isNull");
    this->d->resultCode = value;
}
qint32 ChannelMessage::getMessageId() const
{
    Q_ASSERT_X(!isNull(), "ChannelMessage::getMessageId", "Getter call on object which isNull");
    return this->d->messageId;
}
void ChannelMessage::setMessageId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "ChannelMessage::setMessageId", "Setter call on object which isNull");
    this->d->messageId = value;
}
qint32 ChannelMessage::getAuthorId() const
{
    Q_ASSERT_X(!isNull(), "ChannelMessage::getAuthorId", "Getter call on object which isNull");
    return this->d->authorId;
}
void ChannelMessage::setAuthorId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "ChannelMessage::setAuthorId", "Setter call on object which isNull");
    this->d->authorId = value;
}
QString ChannelMessage::getAuthorLogin() const
{
    Q_ASSERT_X(!isNull(), "ChannelMessage::getAuthorLogin", "Getter call on object which isNull");
    return this->d->authorLogin;
}
void ChannelMessage::setAuthorLogin(QString value)
{
    Q_ASSERT_X(!isNull(), "ChannelMessage::setAuthorLogin", "Setter call on object which isNull");
    this->d->authorLogin = value;
}
QDateTime ChannelMessage::getSendDate() const
{
    Q_ASSERT_X(!isNull(), "ChannelMessage::getSendDate", "Getter call on object which isNull");
    return this->d->sendDate;
}
void ChannelMessage::setSendDate(QDateTime value)
{
    Q_ASSERT_X(!isNull(), "ChannelMessage::setSendDate", "Setter call on object which isNull");
    this->d->sendDate = value;
}
QByteArray ChannelMessage::getData() const
{
    Q_ASSERT_X(!isNull(), "ChannelMessage::getData", "Getter call on object which isNull");
    return this->d->data;
}
void ChannelMessage::setData(QByteArray value)
{
    Q_ASSERT_X(!isNull(), "ChannelMessage::setData", "Setter call on object which isNull");
    this->d->data = value;
}
QString ChannelMessage::getMetadata() const
{
    Q_ASSERT_X(!isNull(), "ChannelMessage::getMetadata", "Getter call on object which isNull");
    return this->d->metadata;
}
void ChannelMessage::setMetadata(QString value)
{
    Q_ASSERT_X(!isNull(), "ChannelMessage::setMetadata", "Setter call on object which isNull");
    this->d->metadata = value;
}

qint32 ChannelMessage::getRepresentedTypeId()
{
    return 29;
}

qint32 ChannelMessage::getTypeId() const
{
    return 29;
}
void ChannelMessage::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20018, this->d->resultCode);
    writer->writeProperty(this, 2, this->d->messageId);
    writer->writeProperty(this, 3, this->d->authorId);
    writer->writeProperty(this, 4, this->d->authorLogin);
    writer->writeProperty(this, 5, this->d->sendDate);
    writer->writeProperty(this, 6, this->d->data);
    writer->writeProperty(this, 7, this->d->metadata);
}
PropertyReadResult ChannelMessage::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->resultCode = (ArtmessageResultCode::ArtmessageResultCodeEnum)reader->readEnum(20018);
            return PropertyReadResult(true);
        case 2:
            this->d->messageId = reader->readInt32();
            return PropertyReadResult(true);
        case 3:
            this->d->authorId = reader->readInt32();
            return PropertyReadResult(true);
        case 4:
            this->d->authorLogin = reader->readString();
            return PropertyReadResult(true);
        case 5:
            this->d->sendDate = reader->readDateTime();
            return PropertyReadResult(true);
        case 6:
            this->d->data = reader->readBytes();
            return PropertyReadResult(true);
        case 7:
            this->d->metadata = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
