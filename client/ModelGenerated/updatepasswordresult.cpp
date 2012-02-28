#include "listwrapperobjects.h"
#include "updatepasswordresult.h"

namespace MoodBox
{

UpdatePasswordResultData::UpdatePasswordResultData() : QSharedData()
{
    this->result = AccountResultCode::Ok;
}
UpdatePasswordResultData::UpdatePasswordResultData(AccountResultCode::AccountResultCodeEnum result) : QSharedData()
{
    this->result = result;
}

UpdatePasswordResultData::~UpdatePasswordResultData()
{
}

UpdatePasswordResult::UpdatePasswordResult() : TransportableObject()
{
}
UpdatePasswordResult::UpdatePasswordResult(AccountResultCode::AccountResultCodeEnum result) : TransportableObject()
{
    d = new UpdatePasswordResultData(result);
}

UpdatePasswordResult::~UpdatePasswordResult()
{
}

void UpdatePasswordResult::resultCall(Callback callback, QVariant state)
{
    UpdatePasswordResultCallbackCaller::call(callback, state, getResult());
}

AccountResultCode::AccountResultCodeEnum UpdatePasswordResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "UpdatePasswordResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void UpdatePasswordResult::setResult(AccountResultCode::AccountResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "UpdatePasswordResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 UpdatePasswordResult::getRepresentedTypeId()
{
    return 10038;
}

qint32 UpdatePasswordResult::getTypeId() const
{
    return 10038;
}
void UpdatePasswordResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20005, this->d->result);
}
PropertyReadResult UpdatePasswordResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
