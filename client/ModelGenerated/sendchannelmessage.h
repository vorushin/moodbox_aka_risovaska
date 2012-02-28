#ifndef SENDCHANNELMESSAGE_H
#define SENDCHANNELMESSAGE_H

#include <QSharedData>
#include <QString>
#include <QByteArray>

#include "transportableobject.h"

namespace MoodBox
{

class SendChannelMessageData : public QSharedData
{
public:
    SendChannelMessageData();
    SendChannelMessageData(qint32 channelId, QByteArray message, QString metadata);
    virtual ~SendChannelMessageData();

    qint32 channelId;
    QByteArray message;
    QString metadata;
};

class SendChannelMessage : public TransportableObject
{
public:
    SendChannelMessage();
    SendChannelMessage(qint32 channelId, QByteArray message, QString metadata);
    virtual ~SendChannelMessage();

protected:
    SendChannelMessage(SendChannelMessageData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static SendChannelMessage* ___new_()
    {
        return new SendChannelMessage(new SendChannelMessageData());
    }
    static SendChannelMessage empty()
    {
        return SendChannelMessage(new SendChannelMessageData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getChannelId() const;
    void setChannelId(qint32 value);
    QByteArray getMessage() const;
    void setMessage(QByteArray value);
    QString getMetadata() const;
    void setMetadata(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<SendChannelMessageData> d;
};

}

#endif // SENDCHANNELMESSAGE_H