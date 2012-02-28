#include "listwrapperobjects.h"
#include "sendprivatemessageresult.h"

namespace MoodBox
{

SendPrivateMessageResultData::SendPrivateMessageResultData() : QSharedData()
{
}
SendPrivateMessageResultData::SendPrivateMessageResultData(SendMessageResult result) : QSharedData()
{
    this->result = result;
}

SendPrivateMessageResultData::~SendPrivateMessageResultData()
{
}

SendPrivateMessageResult::SendPrivateMessageResult() : TransportableObject()
{
}
SendPrivateMessageResult::SendPrivateMessageResult(SendMessageResult result) : TransportableObject()
{
    d = new SendPrivateMessageResultData(result);
}

SendPrivateMessageResult::~SendPrivateMessageResult()
{
}

void SendPrivateMessageResult::resultCall(Callback callback, QVariant state)
{
    SendPrivateMessageResultCallbackCaller::call(callback, state, getResult());
}

SendMessageResult SendPrivateMessageResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "SendPrivateMessageResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void SendPrivateMessageResult::setResult(SendMessageResult value)
{
    Q_ASSERT_X(!isNull(), "SendPrivateMessageResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 SendPrivateMessageResult::getRepresentedTypeId()
{
    return 10024;
}

qint32 SendPrivateMessageResult::getTypeId() const
{
    return 10024;
}
void SendPrivateMessageResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult SendPrivateMessageResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
