#include "listwrapperobjects.h"
#include "resetpasswordresult.h"

namespace MoodBox
{

ResetPasswordResultData::ResetPasswordResultData() : QSharedData()
{
    this->result = AccountResultCode::Ok;
}
ResetPasswordResultData::ResetPasswordResultData(AccountResultCode::AccountResultCodeEnum result) : QSharedData()
{
    this->result = result;
}

ResetPasswordResultData::~ResetPasswordResultData()
{
}

ResetPasswordResult::ResetPasswordResult() : TransportableObject()
{
}
ResetPasswordResult::ResetPasswordResult(AccountResultCode::AccountResultCodeEnum result) : TransportableObject()
{
    d = new ResetPasswordResultData(result);
}

ResetPasswordResult::~ResetPasswordResult()
{
}

void ResetPasswordResult::resultCall(Callback callback, QVariant state)
{
    ResetPasswordResultCallbackCaller::call(callback, state, getResult());
}

AccountResultCode::AccountResultCodeEnum ResetPasswordResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "ResetPasswordResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void ResetPasswordResult::setResult(AccountResultCode::AccountResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "ResetPasswordResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 ResetPasswordResult::getRepresentedTypeId()
{
    return 10064;
}

qint32 ResetPasswordResult::getTypeId() const
{
    return 10064;
}
void ResetPasswordResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20005, this->d->result);
}
PropertyReadResult ResetPasswordResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
