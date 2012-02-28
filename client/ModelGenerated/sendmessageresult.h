#ifndef SENDMESSAGERESULT_H
#define SENDMESSAGERESULT_H

#include <QSharedData>
#include <QDateTime>

#include "transportableobject.h"
#include "contactresultcode.h"

namespace MoodBox
{

class SendMessageResultData : public QSharedData
{
public:
    SendMessageResultData();
    SendMessageResultData(ContactResultCode::ContactResultCodeEnum resultCode, qint32 messageId, QDateTime sendDate);
    virtual ~SendMessageResultData();

    ContactResultCode::ContactResultCodeEnum resultCode;
    qint32 messageId;
    QDateTime sendDate;
};

class SendMessageResult : public TransportableObject
{
public:
    SendMessageResult();
    SendMessageResult(ContactResultCode::ContactResultCodeEnum resultCode, qint32 messageId, QDateTime sendDate);
    virtual ~SendMessageResult();

protected:
    SendMessageResult(SendMessageResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static SendMessageResult* ___new_()
    {
        return new SendMessageResult(new SendMessageResultData());
    }
    static SendMessageResult empty()
    {
        return SendMessageResult(new SendMessageResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    ContactResultCode::ContactResultCodeEnum getResultCode() const;
    void setResultCode(ContactResultCode::ContactResultCodeEnum value);
    qint32 getMessageId() const;
    void setMessageId(qint32 value);
    QDateTime getSendDate() const;
    void setSendDate(QDateTime value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<SendMessageResultData> d;
};

}

#endif // SENDMESSAGERESULT_H