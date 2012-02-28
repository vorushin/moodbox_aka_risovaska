#ifndef SENDFRIENDMESSAGE_H
#define SENDFRIENDMESSAGE_H

#include <QSharedData>
#include <QString>
#include <QByteArray>

#include "transportableobject.h"

namespace MoodBox
{

class SendFriendMessageData : public QSharedData
{
public:
    SendFriendMessageData();
    SendFriendMessageData(bool isPublic, QByteArray message, QString metadata);
    virtual ~SendFriendMessageData();

    bool isPublic;
    QByteArray message;
    QString metadata;
};

class SendFriendMessage : public TransportableObject
{
public:
    SendFriendMessage();
    SendFriendMessage(bool isPublic, QByteArray message, QString metadata);
    virtual ~SendFriendMessage();

protected:
    SendFriendMessage(SendFriendMessageData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static SendFriendMessage* ___new_()
    {
        return new SendFriendMessage(new SendFriendMessageData());
    }
    static SendFriendMessage empty()
    {
        return SendFriendMessage(new SendFriendMessageData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    bool getIsPublic() const;
    void setIsPublic(bool value);
    QByteArray getMessage() const;
    void setMessage(QByteArray value);
    QString getMetadata() const;
    void setMetadata(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<SendFriendMessageData> d;
};

}

#endif // SENDFRIENDMESSAGE_H