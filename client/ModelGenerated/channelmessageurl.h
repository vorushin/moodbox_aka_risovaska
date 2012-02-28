#ifndef CHANNELMESSAGEURL_H
#define CHANNELMESSAGEURL_H

#include <QSharedData>
#include <QDateTime>
#include <QString>

#include "transportableobject.h"
#include "artmessageresultcode.h"

namespace MoodBox
{

class ChannelMessageUrlData : public QSharedData
{
public:
    ChannelMessageUrlData();
    ChannelMessageUrlData(ArtmessageResultCode::ArtmessageResultCodeEnum resultCode, qint32 messageId, qint32 authorId, QString authorLogin, QDateTime sendDate, QString url);
    virtual ~ChannelMessageUrlData();

    ArtmessageResultCode::ArtmessageResultCodeEnum resultCode;
    qint32 messageId;
    qint32 authorId;
    QString authorLogin;
    QDateTime sendDate;
    QString url;
};

class ChannelMessageUrl : public TransportableObject
{
public:
    ChannelMessageUrl();
    ChannelMessageUrl(ArtmessageResultCode::ArtmessageResultCodeEnum resultCode, qint32 messageId, qint32 authorId, QString authorLogin, QDateTime sendDate, QString url);
    virtual ~ChannelMessageUrl();

protected:
    ChannelMessageUrl(ChannelMessageUrlData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static ChannelMessageUrl* ___new_()
    {
        return new ChannelMessageUrl(new ChannelMessageUrlData());
    }
    static ChannelMessageUrl empty()
    {
        return ChannelMessageUrl(new ChannelMessageUrlData());
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
    QString getUrl() const;
    void setUrl(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<ChannelMessageUrlData> d;
};

}

#endif // CHANNELMESSAGEURL_H