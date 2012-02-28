#ifndef SENDPRIVATEMESSAGE_H
#define SENDPRIVATEMESSAGE_H

#include <QSharedData>
#include <QString>
#include <QByteArray>

#include "transportableobject.h"

namespace MoodBox
{

class SendPrivateMessageData : public QSharedData
{
public:
    SendPrivateMessageData();
    SendPrivateMessageData(qint32 recipientId, QByteArray message, QString metadata);
    virtual ~SendPrivateMessageData();

    qint32 recipientId;
    QByteArray message;
    QString metadata;
};

class SendPrivateMessage : public TransportableObject
{
public:
    SendPrivateMessage();
    SendPrivateMessage(qint32 recipientId, QByteArray message, QString metadata);
    virtual ~SendPrivateMessage();

protected:
    SendPrivateMessage(SendPrivateMessageData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static SendPrivateMessage* ___new_()
    {
        return new SendPrivateMessage(new SendPrivateMessageData());
    }
    static SendPrivateMessage empty()
    {
        return SendPrivateMessage(new SendPrivateMessageData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getRecipientId() const;
    void setRecipientId(qint32 value);
    QByteArray getMessage() const;
    void setMessage(QByteArray value);
    QString getMetadata() const;
    void setMetadata(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<SendPrivateMessageData> d;
};

}

#endif // SENDPRIVATEMESSAGE_H