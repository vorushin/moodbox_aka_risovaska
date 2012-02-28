#ifndef ARTMESSAGE_H
#define ARTMESSAGE_H

#include <QSharedData>
#include <QDateTime>
#include <QString>
#include <QByteArray>

#include "transportableobject.h"
#include "messagetype.h"
#include "artmessageresultcode.h"

namespace MoodBox
{

class ArtMessageData : public QSharedData
{
public:
    ArtMessageData();
    ArtMessageData(qint32 messageId, MessageType::MessageTypeEnum type, qint32 authorId, QDateTime sendDate, QByteArray data, ArtmessageResultCode::ArtmessageResultCodeEnum resultCode, QString metadata, QString url);
    virtual ~ArtMessageData();

    qint32 messageId;
    MessageType::MessageTypeEnum type;
    qint32 authorId;
    QDateTime sendDate;
    QByteArray data;
    ArtmessageResultCode::ArtmessageResultCodeEnum resultCode;
    QString metadata;
    QString url;
};

class ArtMessage : public TransportableObject
{
public:
    ArtMessage();
    ArtMessage(qint32 messageId, MessageType::MessageTypeEnum type, qint32 authorId, QDateTime sendDate, QByteArray data, ArtmessageResultCode::ArtmessageResultCodeEnum resultCode, QString metadata, QString url);
    virtual ~ArtMessage();

protected:
    ArtMessage(ArtMessageData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static ArtMessage* ___new_()
    {
        return new ArtMessage(new ArtMessageData());
    }
    static ArtMessage empty()
    {
        return ArtMessage(new ArtMessageData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getMessageId() const;
    void setMessageId(qint32 value);
    MessageType::MessageTypeEnum getType() const;
    void setType(MessageType::MessageTypeEnum value);
    qint32 getAuthorId() const;
    void setAuthorId(qint32 value);
    QDateTime getSendDate() const;
    void setSendDate(QDateTime value);
    QByteArray getData() const;
    void setData(QByteArray value);
    ArtmessageResultCode::ArtmessageResultCodeEnum getResultCode() const;
    void setResultCode(ArtmessageResultCode::ArtmessageResultCodeEnum value);
    QString getMetadata() const;
    void setMetadata(QString value);
    QString getUrl() const;
    void setUrl(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<ArtMessageData> d;
};

}

#endif // ARTMESSAGE_H