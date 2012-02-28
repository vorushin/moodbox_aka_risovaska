#ifndef DELETEMESSAGE_H
#define DELETEMESSAGE_H

#include <QSharedData>

#include "transportableobject.h"
#include "messagetype.h"

namespace MoodBox
{

class DeleteMessageData : public QSharedData
{
public:
    DeleteMessageData();
    DeleteMessageData(qint32 contactId, MessageType::MessageTypeEnum messageType, qint32 messageId);
    virtual ~DeleteMessageData();

    qint32 contactId;
    MessageType::MessageTypeEnum messageType;
    qint32 messageId;
};

class DeleteMessage : public TransportableObject
{
public:
    DeleteMessage();
    DeleteMessage(qint32 contactId, MessageType::MessageTypeEnum messageType, qint32 messageId);
    virtual ~DeleteMessage();

protected:
    DeleteMessage(DeleteMessageData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static DeleteMessage* ___new_()
    {
        return new DeleteMessage(new DeleteMessageData());
    }
    static DeleteMessage empty()
    {
        return DeleteMessage(new DeleteMessageData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getContactId() const;
    void setContactId(qint32 value);
    MessageType::MessageTypeEnum getMessageType() const;
    void setMessageType(MessageType::MessageTypeEnum value);
    qint32 getMessageId() const;
    void setMessageId(qint32 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<DeleteMessageData> d;
};

}

#endif // DELETEMESSAGE_H