#include "listwrapperobjects.h"
#include "createaccountresult.h"

namespace MoodBox
{

CreateAccountResultData::CreateAccountResultData() : QSharedData()
{
    this->result = AccountResultCode::Ok;
}
CreateAccountResultData::CreateAccountResultData(AccountResultCode::AccountResultCodeEnum result) : QSharedData()
{
    this->result = result;
}

CreateAccountResultData::~CreateAccountResultData()
{
}

CreateAccountResult::CreateAccountResult() : TransportableObject()
{
}
CreateAccountResult::CreateAccountResult(AccountResultCode::AccountResultCodeEnum result) : TransportableObject()
{
    d = new CreateAccountResultData(result);
}

CreateAccountResult::~CreateAccountResult()
{
}

void CreateAccountResult::resultCall(Callback callback, QVariant state)
{
    CreateAccountResultCallbackCaller::call(callback, state, getResult());
}

AccountResultCode::AccountResultCodeEnum CreateAccountResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "CreateAccountResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void CreateAccountResult::setResult(AccountResultCode::AccountResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "CreateAccountResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 CreateAccountResult::getRepresentedTypeId()
{
    return 10004;
}

qint32 CreateAccountResult::getTypeId() const
{
    return 10004;
}
void CreateAccountResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20005, this->d->result);
}
PropertyReadResult CreateAccountResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
