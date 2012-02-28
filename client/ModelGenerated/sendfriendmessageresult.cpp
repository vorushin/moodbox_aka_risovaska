#include "listwrapperobjects.h"
#include "sendfriendmessageresult.h"

namespace MoodBox
{

SendFriendMessageResultData::SendFriendMessageResultData() : QSharedData()
{
}
SendFriendMessageResultData::SendFriendMessageResultData(SendMessageResult result) : QSharedData()
{
    this->result = result;
}

SendFriendMessageResultData::~SendFriendMessageResultData()
{
}

SendFriendMessageResult::SendFriendMessageResult() : TransportableObject()
{
}
SendFriendMessageResult::SendFriendMessageResult(SendMessageResult result) : TransportableObject()
{
    d = new SendFriendMessageResultData(result);
}

SendFriendMessageResult::~SendFriendMessageResult()
{
}

void SendFriendMessageResult::resultCall(Callback callback, QVariant state)
{
    SendFriendMessageResultCallbackCaller::call(callback, state, getResult());
}

SendMessageResult SendFriendMessageResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "SendFriendMessageResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void SendFriendMessageResult::setResult(SendMessageResult value)
{
    Q_ASSERT_X(!isNull(), "SendFriendMessageResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 SendFriendMessageResult::getRepresentedTypeId()
{
    return 10022;
}

qint32 SendFriendMessageResult::getTypeId() const
{
    return 10022;
}
void SendFriendMessageResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult SendFriendMessageResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = SendMessageResult::empty();
            return PropertyReadResult(&this->d->result);
    }

    return PropertyReadResult(false);
}

}
