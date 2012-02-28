#include "listwrapperobjects.h"
#include "obscenechannelmessageresult.h"

namespace MoodBox
{

ObsceneChannelMessageResultData::ObsceneChannelMessageResultData() : QSharedData()
{
    this->result = StandartResultCode::Undefined;
}
ObsceneChannelMessageResultData::ObsceneChannelMessageResultData(StandartResultCode::StandartResultCodeEnum result) : QSharedData()
{
    this->result = result;
}

ObsceneChannelMessageResultData::~ObsceneChannelMessageResultData()
{
}

ObsceneChannelMessageResult::ObsceneChannelMessageResult() : TransportableObject()
{
}
ObsceneChannelMessageResult::ObsceneChannelMessageResult(StandartResultCode::StandartResultCodeEnum result) : TransportableObject()
{
    d = new ObsceneChannelMessageResultData(result);
}

ObsceneChannelMessageResult::~ObsceneChannelMessageResult()
{
}

void ObsceneChannelMessageResult::resultCall(Callback callback, QVariant state)
{
    ObsceneChannelMessageResultCallbackCaller::call(callback, state, getResult());
}

StandartResultCode::StandartResultCodeEnum ObsceneChannelMessageResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "ObsceneChannelMessageResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void ObsceneChannelMessageResult::setResult(StandartResultCode::StandartResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "ObsceneChannelMessageResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 ObsceneChannelMessageResult::getRepresentedTypeId()
{
    return 10026;
}

qint32 ObsceneChannelMessageResult::getTypeId() const
{
    return 10026;
}
void ObsceneChannelMessageResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20022, this->d->result);
}
PropertyReadResult ObsceneChannelMessageResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = (StandartResultCode::StandartResultCodeEnum)reader->readEnum(20022);
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
