#include "listwrapperobjects.h"
#include "getnextchannelmessageurlresult.h"

namespace MoodBox
{

GetNextChannelMessageUrlResultData::GetNextChannelMessageUrlResultData() : QSharedData()
{
}
GetNextChannelMessageUrlResultData::GetNextChannelMessageUrlResultData(ChannelMessageUrl result) : QSharedData()
{
    this->result = result;
}

GetNextChannelMessageUrlResultData::~GetNextChannelMessageUrlResultData()
{
}

GetNextChannelMessageUrlResult::GetNextChannelMessageUrlResult() : TransportableObject()
{
}
GetNextChannelMessageUrlResult::GetNextChannelMessageUrlResult(ChannelMessageUrl result) : TransportableObject()
{
    d = new GetNextChannelMessageUrlResultData(result);
}

GetNextChannelMessageUrlResult::~GetNextChannelMessageUrlResult()
{
}

void GetNextChannelMessageUrlResult::resultCall(Callback callback, QVariant state)
{
    GetNextChannelMessageUrlResultCallbackCaller::call(callback, state, getResult());
}

ChannelMessageUrl GetNextChannelMessageUrlResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "GetNextChannelMessageUrlResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void GetNextChannelMessageUrlResult::setResult(ChannelMessageUrl value)
{
    Q_ASSERT_X(!isNull(), "GetNextChannelMessageUrlResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 GetNextChannelMessageUrlResult::getRepresentedTypeId()
{
    return 10008;
}

qint32 GetNextChannelMessageUrlResult::getTypeId() const
{
    return 10008;
}
void GetNextChannelMessageUrlResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult GetNextChannelMessageUrlResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = ChannelMessageUrl::empty();
            return PropertyReadResult(&this->d->result);
    }

    return PropertyReadResult(false);
}

}
