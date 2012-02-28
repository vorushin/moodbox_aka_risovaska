#include "listwrapperobjects.h"
#include "getnextchannelmessageresult.h"

namespace MoodBox
{

GetNextChannelMessageResultData::GetNextChannelMessageResultData() : QSharedData()
{
}
GetNextChannelMessageResultData::GetNextChannelMessageResultData(ChannelMessage result) : QSharedData()
{
    this->result = result;
}

GetNextChannelMessageResultData::~GetNextChannelMessageResultData()
{
}

GetNextChannelMessageResult::GetNextChannelMessageResult() : TransportableObject()
{
}
GetNextChannelMessageResult::GetNextChannelMessageResult(ChannelMessage result) : TransportableObject()
{
    d = new GetNextChannelMessageResultData(result);
}

GetNextChannelMessageResult::~GetNextChannelMessageResult()
{
}

void GetNextChannelMessageResult::resultCall(Callback callback, QVariant state)
{
    GetNextChannelMessageResultCallbackCaller::call(callback, state, getResult());
}

ChannelMessage GetNextChannelMessageResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "GetNextChannelMessageResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void GetNextChannelMessageResult::setResult(ChannelMessage value)
{
    Q_ASSERT_X(!isNull(), "GetNextChannelMessageResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 GetNextChannelMessageResult::getRepresentedTypeId()
{
    return 10108;
}

qint32 GetNextChannelMessageResult::getTypeId() const
{
    return 10108;
}
void GetNextChannelMessageResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult GetNextChannelMessageResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = ChannelMessage::empty();
            return PropertyReadResult(&this->d->result);
    }

    return PropertyReadResult(false);
}

}
