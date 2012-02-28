#ifndef DELETEMESSAGECOMMAND_H
#define DELETEMESSAGECOMMAND_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class DeleteMessageCommandData : public QSharedData
{
public:
    DeleteMessageCommandData();
    DeleteMessageCommandData(qint32 contactId, qint32 messageId);
    virtual ~DeleteMessageCommandData();

    qint32 contactId;
    qint32 messageId;
};

class DeleteMessageCommand : public TransportableObject
{
public:
    DeleteMessageCommand();
    DeleteMessageCommand(qint32 contactId, qint32 messageId);
    virtual ~DeleteMessageCommand();

protected:
    DeleteMessageCommand(DeleteMessageCommandData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static DeleteMessageCommand* ___new_()
    {
        return new DeleteMessageCommand(new DeleteMessageCommandData());
    }
    static DeleteMessageCommand empty()
    {
        return DeleteMessageCommand(new DeleteMessageCommandData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getContactId() const;
    void setContactId(qint32 value);
    qint32 getMessageId() const;
    void setMessageId(qint32 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<DeleteMessageCommandData> d;
};

}

#endif // DELETEMESSAGECOMMAND_H