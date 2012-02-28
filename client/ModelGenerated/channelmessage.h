#ifndef CHANNELMESSAGE_H
#define CHANNELMESSAGE_H

#include <QSharedData>
#include <QDateTime>
#include <QString>
#include <QByteArray>

#include "transportableobject.h"
#include "artmessageresultcode.h"

namespace MoodBox
{

class ChannelMessageData : public QSharedData
{
public:
    ChannelMessageData();
    ChannelMessageData(ArtmessageResultCode::ArtmessageResultCodeEnum resultCode, qint32 messageId, qint32 authorId, QString authorLogin, QDateTime sendDate, QByteArray data, QString metadata);
    virtual ~ChannelMessageData();

    ArtmessageResultCode::ArtmessageResultCodeEnum resultCode;
    qint32 messageId;
    qint32 authorId;
    QString authorLogin;
    QDateTime sendDate;
    QByteArray data;
    QString metadata;
};

class ChannelMessage : public TransportableObject
{
public:
    ChannelMessage();
    ChannelMessage(ArtmessageResultCode::ArtmessageResultCodeEnum resultCode, qint32 messageId, qint32 authorId, QString authorLogin, QDateTime sendDate, QByteArray data, QString metadata);
    virtual ~ChannelMessage();

protected:
    ChannelMessage(ChannelMessageData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static ChannelMessage* ___new_()
    {
        return new ChannelMessage(new ChannelMessageData());
    }
    static ChannelMessage empty()
    {
        return ChannelMessage(new ChannelMessageData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    ArtmessageResultCode::ArtmessageResultCodeEnum getResultCode() const;
    void setResultCode(ArtmessageResultCode::ArtmessageResultCodeEnum value);
    qint32 getMessageId() const;
    void setMessageId(qint32 value);
    qint32 getAuthorId() const;
    void setAuthorId(qint32 value);
    QString getAuthorLogin() const;
    void setAuthorLogin(QString value);
    QDateTime getSendDate() const;
    void setSendDate(QDateTime value);
    QByteArray getData() const;
    void setData(QByteArray value);
    QString getMetadata() const;
    void setMetadata(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<ChannelMessageData> d;
};

}

#endif // CHANNELMESSAGE_H