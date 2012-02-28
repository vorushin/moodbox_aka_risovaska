#include "listwrapperobjects.h"
#include "deletemessage.h"

namespace MoodBox
{

DeleteMessageData::DeleteMessageData() : QSharedData()
{
    this->contactId = 0;
    this->messageType = MessageType::Undefined;
    this->messageId = 0;
}
DeleteMessageData::DeleteMessageData(qint32 contactId, MessageType::MessageTypeEnum messageType, qint32 messageId) : QSharedData()
{
    this->contactId = contactId;
    this->messageType = messageType;
    this->messageId = messageId;
}

DeleteMessageData::~DeleteMessageData()
{
}

DeleteMessage::DeleteMessage() : TransportableObject()
{
}
DeleteMessage::DeleteMessage(qint32 contactId, MessageType::MessageTypeEnum messageType, qint32 messageId) : TransportableObject()
{
    d = new DeleteMessageData(contactId, messageType, messageId);
}

DeleteMessage::~DeleteMessage()
{
}

qint32 DeleteMessage::getContactId() const
{
    Q_ASSERT_X(!isNull(), "DeleteMessage::getContactId", "Getter call on object which isNull");
    return this->d->contactId;
}
void DeleteMessage::setContactId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "DeleteMessage::setContactId", "Setter call on object which isNull");
    this->d->contactId = value;
}
MessageType::MessageTypeEnum DeleteMessage::getMessageType() const
{
    Q_ASSERT_X(!isNull(), "DeleteMessage::getMessageType", "Getter call on object which isNull");
    return this->d->messageType;
}
void DeleteMessage::setMessageType(MessageType::MessageTypeEnum value)
{
    Q_ASSERT_X(!isNull(), "DeleteMessage::setMessageType", "Setter call on object which isNull");
    this->d->messageType = value;
}
qint32 DeleteMessage::getMessageId() const
{
    Q_ASSERT_X(!isNull(), "DeleteMessage::getMessageId", "Getter call on object which isNull");
    return this->d->messageId;
}
void DeleteMessage::setMessageId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "DeleteMessage::setMessageId", "Setter call on object which isNull");
    this->d->messageId = value;
}

qint32 DeleteMessage::getRepresentedTypeId()
{
    return 10047;
}

qint32 DeleteMessage::getTypeId() const
{
    return 10047;
}
void DeleteMessage::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->contactId);
    writer->writeEnumProperty(this, 2, 20007, this->d->messageType);
    writer->writeProperty(this, 3, this->d->messageId);
}
PropertyReadResult DeleteMessage::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->contactId = reader->readInt32();
            return PropertyReadResult(true);
        case 2:
            this->d->messageType = (MessageType::MessageTypeEnum)reader->readEnum(20007);
            return PropertyReadResult(true);
        case 3:
            this->d->messageId = reader->readInt32();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
