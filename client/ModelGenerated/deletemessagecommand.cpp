#include "listwrapperobjects.h"
#include "deletemessagecommand.h"

namespace MoodBox
{

DeleteMessageCommandData::DeleteMessageCommandData() : QSharedData()
{
    this->contactId = 0;
    this->messageId = 0;
}
DeleteMessageCommandData::DeleteMessageCommandData(qint32 contactId, qint32 messageId) : QSharedData()
{
    this->contactId = contactId;
    this->messageId = messageId;
}

DeleteMessageCommandData::~DeleteMessageCommandData()
{
}

DeleteMessageCommand::DeleteMessageCommand() : TransportableObject()
{
}
DeleteMessageCommand::DeleteMessageCommand(qint32 contactId, qint32 messageId) : TransportableObject()
{
    d = new DeleteMessageCommandData(contactId, messageId);
}

DeleteMessageCommand::~DeleteMessageCommand()
{
}

qint32 DeleteMessageCommand::getContactId() const
{
    Q_ASSERT_X(!isNull(), "DeleteMessageCommand::getContactId", "Getter call on object which isNull");
    return this->d->contactId;
}
void DeleteMessageCommand::setContactId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "DeleteMessageCommand::setContactId", "Setter call on object which isNull");
    this->d->contactId = value;
}
qint32 DeleteMessageCommand::getMessageId() const
{
    Q_ASSERT_X(!isNull(), "DeleteMessageCommand::getMessageId", "Getter call on object which isNull");
    return this->d->messageId;
}
void DeleteMessageCommand::setMessageId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "DeleteMessageCommand::setMessageId", "Setter call on object which isNull");
    this->d->messageId = value;
}

qint32 DeleteMessageCommand::getRepresentedTypeId()
{
    return 34;
}

qint32 DeleteMessageCommand::getTypeId() const
{
    return 34;
}
void DeleteMessageCommand::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->contactId);
    writer->writeProperty(this, 2, this->d->messageId);
}
PropertyReadResult DeleteMessageCommand::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
            this->d->messageId = reader->readInt32();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
