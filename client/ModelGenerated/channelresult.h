#ifndef CHANNELRESULT_H
#define CHANNELRESULT_H

#include <QSharedData>
#include <QList>
#include <QString>
#include <QDate>

#include "transportableobject.h"
#include "contactlogin.h"

namespace MoodBox
{

class ChannelResultData : public QSharedData
{
public:
    ChannelResultData();
    ChannelResultData(qint32 channelId, qint32 authorId, QString authorLogin, QDate creationDate, QString title, QString shortDescription, QString fullDescription, qint32 userCount, QString logoUrl, QList<ContactLogin> moderators);
    virtual ~ChannelResultData();

    qint32 channelId;
    qint32 authorId;
    QString authorLogin;
    QDate creationDate;
    QString title;
    QString shortDescription;
    QString fullDescription;
    qint32 userCount;
    QString logoUrl;
    QList<ContactLogin> moderators;
};

class ChannelResult : public TransportableObject
{
public:
    ChannelResult();
    ChannelResult(qint32 channelId, qint32 authorId, QString authorLogin, QDate creationDate, QString title, QString shortDescription, QString fullDescription, qint32 userCount, QString logoUrl, QList<ContactLogin> moderators);
    virtual ~ChannelResult();

protected:
    ChannelResult(ChannelResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static ChannelResult* ___new_()
    {
        return new ChannelResult(new ChannelResultData());
    }
    static ChannelResult empty()
    {
        return ChannelResult(new ChannelResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getChannelId() const;
    void setChannelId(qint32 value);
    qint32 getAuthorId() const;
    void setAuthorId(qint32 value);
    QString getAuthorLogin() const;
    void setAuthorLogin(QString value);
    QDate getCreationDate() const;
    void setCreationDate(QDate value);
    QString getTitle() const;
    void setTitle(QString value);
    QString getShortDescription() const;
    void setShortDescription(QString value);
    QString getFullDescription() const;
    void setFullDescription(QString value);
    qint32 getUserCount() const;
    void setUserCount(qint32 value);
    QString getLogoUrl() const;
    void setLogoUrl(QString value);
    QList<ContactLogin> getModerators() const;
    void setModerators(QList<ContactLogin> value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<ChannelResultData> d;
};

}

#endif // CHANNELRESULT_H