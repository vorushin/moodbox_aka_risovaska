#include "listwrapperobjects.h"
#include "getcommandsresult.h"

namespace MoodBox
{

GetCommandsResultData::GetCommandsResultData() : QSharedData()
{
}
GetCommandsResultData::GetCommandsResultData(CommandPackage result) : QSharedData()
{
    this->result = result;
}

GetCommandsResultData::~GetCommandsResultData()
{
}

GetCommandsResult::GetCommandsResult() : TransportableObject()
{
}
GetCommandsResult::GetCommandsResult(CommandPackage result) : TransportableObject()
{
    d = new GetCommandsResultData(result);
}

GetCommandsResult::~GetCommandsResult()
{
}

void GetCommandsResult::resultCall(Callback callback, QVariant state)
{
    GetCommandsResultCallbackCaller::call(callback, state, getResult());
}

CommandPackage GetCommandsResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "GetCommandsResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void GetCommandsResult::setResult(CommandPackage value)
{
    Q_ASSERT_X(!isNull(), "GetCommandsResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 GetCommandsResult::getRepresentedTypeId()
{
    return 10050;
}

qint32 GetCommandsResult::getTypeId() const
{
    return 10050;
}
void GetCommandsResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult GetCommandsResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = CommandPackage::empty();
            return PropertyReadResult(&this->d->result);
    }

    return PropertyReadResult(false);
}

}
