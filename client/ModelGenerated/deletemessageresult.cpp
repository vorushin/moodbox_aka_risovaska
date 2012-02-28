#include "listwrapperobjects.h"
#include "deletemessageresult.h"

namespace MoodBox
{

DeleteMessageResultData::DeleteMessageResultData() : QSharedData()
{
    this->result = StandartResultCode::Undefined;
}
DeleteMessageResultData::DeleteMessageResultData(StandartResultCode::StandartResultCodeEnum result) : QSharedData()
{
    this->result = result;
}

DeleteMessageResultData::~DeleteMessageResultData()
{
}

DeleteMessageResult::DeleteMessageResult() : TransportableObject()
{
}
DeleteMessageResult::DeleteMessageResult(StandartResultCode::StandartResultCodeEnum result) : TransportableObject()
{
    d = new DeleteMessageResultData(result);
}

DeleteMessageResult::~DeleteMessageResult()
{
}

void DeleteMessageResult::resultCall(Callback callback, QVariant state)
{
    DeleteMessageResultCallbackCaller::call(callback, state, getResult());
}

StandartResultCode::StandartResultCodeEnum DeleteMessageResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "DeleteMessageResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void DeleteMessageResult::setResult(StandartResultCode::StandartResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "DeleteMessageResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 DeleteMessageResult::getRepresentedTypeId()
{
    return 10048;
}

qint32 DeleteMessageResult::getTypeId() const
{
    return 10048;
}
void DeleteMessageResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20022, this->d->result);
}
PropertyReadResult DeleteMessageResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
