#include "listwrapperobjects.h"
#include "getauthorizationresult.h"

namespace MoodBox
{

GetAuthorizationResultData::GetAuthorizationResultData() : QSharedData()
{
}
GetAuthorizationResultData::GetAuthorizationResultData(Authorization result) : QSharedData()
{
    this->result = result;
}

GetAuthorizationResultData::~GetAuthorizationResultData()
{
}

GetAuthorizationResult::GetAuthorizationResult() : TransportableObject()
{
}
GetAuthorizationResult::GetAuthorizationResult(Authorization result) : TransportableObject()
{
    d = new GetAuthorizationResultData(result);
}

GetAuthorizationResult::~GetAuthorizationResult()
{
}

void GetAuthorizationResult::resultCall(Callback callback, QVariant state)
{
    GetAuthorizationResultCallbackCaller::call(callback, state, getResult());
}

Authorization GetAuthorizationResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "GetAuthorizationResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void GetAuthorizationResult::setResult(Authorization value)
{
    Q_ASSERT_X(!isNull(), "GetAuthorizationResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 GetAuthorizationResult::getRepresentedTypeId()
{
    return 10086;
}

qint32 GetAuthorizationResult::getTypeId() const
{
    return 10086;
}
void GetAuthorizationResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult GetAuthorizationResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = Authorization::empty();
            return PropertyReadResult(&this->d->result);
    }

    return PropertyReadResult(false);
}

}
