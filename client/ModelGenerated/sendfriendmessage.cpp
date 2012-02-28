#include "listwrapperobjects.h"
#include "sendfriendmessage.h"

namespace MoodBox
{

SendFriendMessageData::SendFriendMessageData() : QSharedData()
{
    this->isPublic = false;
}
SendFriendMessageData::SendFriendMessageData(bool isPublic, QByteArray message, QString metadata) : QSharedData()
{
    this->isPublic = isPublic;
    this->message = message;
    this->metadata = metadata;
}

SendFriendMessageData::~SendFriendMessageData()
{
}

SendFriendMessage::SendFriendMessage() : TransportableObject()
{
}
SendFriendMessage::SendFriendMessage(bool isPublic, QByteArray message, QString metadata) : TransportableObject()
{
    d = new SendFriendMessageData(isPublic, message, metadata);
}

SendFriendMessage::~SendFriendMessage()
{
}

bool SendFriendMessage::getIsPublic() const
{
    Q_ASSERT_X(!isNull(), "SendFriendMessage::getIsPublic", "Getter call on object which isNull");
    return this->d->isPublic;
}
void SendFriendMessage::setIsPublic(bool value)
{
    Q_ASSERT_X(!isNull(), "SendFriendMessage::setIsPublic", "Setter call on object which isNull");
    this->d->isPublic = value;
}
QByteArray SendFriendMessage::getMessage() const
{
    Q_ASSERT_X(!isNull(), "SendFriendMessage::getMessage", "Getter call on object which isNull");
    return this->d->message;
}
void SendFriendMessage::setMessage(QByteArray value)
{
    Q_ASSERT_X(!isNull(), "SendFriendMessage::setMessage", "Setter call on object which isNull");
    this->d->message = value;
}
QString SendFriendMessage::getMetadata() const
{
    Q_ASSERT_X(!isNull(), "SendFriendMessage::getMetadata", "Getter call on object which isNull");
    return this->d->metadata;
}
void SendFriendMessage::setMetadata(QString value)
{
    Q_ASSERT_X(!isNull(), "SendFriendMessage::setMetadata", "Setter call on object which isNull");
    this->d->metadata = value;
}

qint32 SendFriendMessage::getRepresentedTypeId()
{
    return 10021;
}

qint32 SendFriendMessage::getTypeId() const
{
    return 10021;
}
void SendFriendMessage::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->isPublic);
    writer->writeProperty(this, 2, this->d->message);
    writer->writeProperty(this, 3, this->d->metadata);
}
PropertyReadResult SendFriendMessage::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->isPublic = reader->readBool();
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
