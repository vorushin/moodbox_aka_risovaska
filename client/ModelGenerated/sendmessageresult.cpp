#include "listwrapperobjects.h"
#include "sendmessageresult.h"

namespace MoodBox
{

SendMessageResultData::SendMessageResultData() : QSharedData()
{
    this->resultCode = ContactResultCode::Ok;
    this->messageId = 0;
}
SendMessageResultData::SendMessageResultData(ContactResultCode::ContactResultCodeEnum resultCode, qint32 messageId, QDateTime sendDate) : QSharedData()
{
    this->resultCode = resultCode;
    this->messageId = messageId;
    this->sendDate = sendDate;
}

SendMessageResultData::~SendMessageResultData()
{
}

SendMessageResult::SendMessageResult() : TransportableObject()
{
}
SendMessageResult::SendMessageResult(ContactResultCode::ContactResultCodeEnum resultCode, qint32 messageId, QDateTime sendDate) : TransportableObject()
{
    d = new SendMessageResultData(resultCode, messageId, sendDate);
}

SendMessageResult::~SendMessageResult()
{
}

ContactResultCode::ContactResultCodeEnum SendMessageResult::getResultCode() const
{
    Q_ASSERT_X(!isNull(), "SendMessageResult::getResultCode", "Getter call on object which isNull");
    return this->d->resultCode;
}
void SendMessageResult::setResultCode(ContactResultCode::ContactResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "SendMessageResult::setResultCode", "Setter call on object which isNull");
    this->d->resultCode = value;
}
qint32 SendMessageResult::getMessageId() const
{
    Q_ASSERT_X(!isNull(), "SendMessageResult::getMessageId", "Getter call on object which isNull");
    return this->d->messageId;
}
void SendMessageResult::setMessageId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "SendMessageResult::setMessageId", "Setter call on object which isNull");
    this->d->messageId = value;
}
QDateTime SendMessageResult::getSendDate() const
{
    Q_ASSERT_X(!isNull(), "SendMessageResult::getSendDate", "Getter call on object which isNull");
    return this->d->sendDate;
}
void SendMessageResult::setSendDate(QDateTime value)
{
    Q_ASSERT_X(!isNull(), "SendMessageResult::setSendDate", "Setter call on object which isNull");
    this->d->sendDate = value;
}

qint32 SendMessageResult::getRepresentedTypeId()
{
    return 21;
}

qint32 SendMessageResult::getTypeId() const
{
    return 21;
}
void SendMessageResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20009, this->d->resultCode);
    writer->writeProperty(this, 2, this->d->messageId);
    writer->writeProperty(this, 3, this->d->sendDate);
}
PropertyReadResult SendMessageResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->resultCode = (ContactResultCode::ContactResultCodeEnum)reader->readEnum(20009);
            return PropertyReadResult(true);
        case 2:
            this->d->messageId = reader->readInt32();
            return PropertyReadResult(true);
        case 3:
            this->d->sendDate = reader->readDateTime();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
