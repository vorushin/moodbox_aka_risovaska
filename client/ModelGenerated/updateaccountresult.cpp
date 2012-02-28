#include "listwrapperobjects.h"
#include "updateaccountresult.h"

namespace MoodBox
{

UpdateAccountResultData::UpdateAccountResultData() : QSharedData()
{
    this->result = AccountResultCode::Ok;
}
UpdateAccountResultData::UpdateAccountResultData(AccountResultCode::AccountResultCodeEnum result) : QSharedData()
{
    this->result = result;
}

UpdateAccountResultData::~UpdateAccountResultData()
{
}

UpdateAccountResult::UpdateAccountResult() : TransportableObject()
{
}
UpdateAccountResult::UpdateAccountResult(AccountResultCode::AccountResultCodeEnum result) : TransportableObject()
{
    d = new UpdateAccountResultData(result);
}

UpdateAccountResult::~UpdateAccountResult()
{
}

void UpdateAccountResult::resultCall(Callback callback, QVariant state)
{
    UpdateAccountResultCallbackCaller::call(callback, state, getResult());
}

AccountResultCode::AccountResultCodeEnum UpdateAccountResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "UpdateAccountResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void UpdateAccountResult::setResult(AccountResultCode::AccountResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "UpdateAccountResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 UpdateAccountResult::getRepresentedTypeId()
{
    return 10006;
}

qint32 UpdateAccountResult::getTypeId() const
{
    return 10006;
}
void UpdateAccountResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20005, this->d->result);
}
PropertyReadResult UpdateAccountResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = (AccountResultCode::AccountResultCodeEnum)reader->readEnum(20005);
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
