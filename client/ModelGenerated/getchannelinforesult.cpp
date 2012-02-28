#include "listwrapperobjects.h"
#include "getchannelinforesult.h"

namespace MoodBox
{

GetChannelInfoResultData::GetChannelInfoResultData() : QSharedData()
{
}
GetChannelInfoResultData::GetChannelInfoResultData(ChannelResult result) : QSharedData()
{
    this->result = result;
}

GetChannelInfoResultData::~GetChannelInfoResultData()
{
}

GetChannelInfoResult::GetChannelInfoResult() : TransportableObject()
{
}
GetChannelInfoResult::GetChannelInfoResult(ChannelResult result) : TransportableObject()
{
    d = new GetChannelInfoResultData(result);
}

GetChannelInfoResult::~GetChannelInfoResult()
{
}

void GetChannelInfoResult::resultCall(Callback callback, QVariant state)
{
    GetChannelInfoResultCallbackCaller::call(callback, state, getResult());
}

ChannelResult GetChannelInfoResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "GetChannelInfoResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void GetChannelInfoResult::setResult(ChannelResult value)
{
    Q_ASSERT_X(!isNull(), "GetChannelInfoResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 GetChannelInfoResult::getRepresentedTypeId()
{
    return 10104;
}

qint32 GetChannelInfoResult::getTypeId() const
{
    return 10104;
}
void GetChannelInfoResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult GetChannelInfoResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = ChannelResult::empty();
            return PropertyReadResult(&this->d->result);
    }

    return PropertyReadResult(false);
}

}
