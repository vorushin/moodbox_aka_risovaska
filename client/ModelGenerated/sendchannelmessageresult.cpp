#include "listwrapperobjects.h"
#include "sendchannelmessageresult.h"

namespace MoodBox
{

SendChannelMessageResultData::SendChannelMessageResultData() : QSharedData()
{
}
SendChannelMessageResultData::SendChannelMessageResultData(SendMessageResult result) : QSharedData()
{
    this->result = result;
}

SendChannelMessageResultData::~SendChannelMessageResultData()
{
}

SendChannelMessageResult::SendChannelMessageResult() : TransportableObject()
{
}
SendChannelMessageResult::SendChannelMessageResult(SendMessageResult result) : TransportableObject()
{
    d = new SendChannelMessageResultData(result);
}

SendChannelMessageResult::~SendChannelMessageResult()
{
}

void SendChannelMessageResult::resultCall(Callback callback, QVariant state)
{
    SendChannelMessageResultCallbackCaller::call(callback, state, getResult());
}

SendMessageResult SendChannelMessageResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "SendChannelMessageResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void SendChannelMessageResult::setResult(SendMessageResult value)
{
    Q_ASSERT_X(!isNull(), "SendChannelMessageResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 SendChannelMessageResult::getRepresentedTypeId()
{
    return 10106;
}

qint32 SendChannelMessageResult::getTypeId() const
{
    return 10106;
}
void SendChannelMessageResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult SendChannelMessageResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
