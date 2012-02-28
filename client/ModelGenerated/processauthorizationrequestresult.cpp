#include "listwrapperobjects.h"
#include "processauthorizationrequestresult.h"

namespace MoodBox
{

ProcessAuthorizationRequestResultData::ProcessAuthorizationRequestResultData() : QSharedData()
{
    this->result = AuthorizationResultCode::Ok;
}
ProcessAuthorizationRequestResultData::ProcessAuthorizationRequestResultData(AuthorizationResultCode::AuthorizationResultCodeEnum result) : QSharedData()
{
    this->result = result;
}

ProcessAuthorizationRequestResultData::~ProcessAuthorizationRequestResultData()
{
}

ProcessAuthorizationRequestResult::ProcessAuthorizationRequestResult() : TransportableObject()
{
}
ProcessAuthorizationRequestResult::ProcessAuthorizationRequestResult(AuthorizationResultCode::AuthorizationResultCodeEnum result) : TransportableObject()
{
    d = new ProcessAuthorizationRequestResultData(result);
}

ProcessAuthorizationRequestResult::~ProcessAuthorizationRequestResult()
{
}

void ProcessAuthorizationRequestResult::resultCall(Callback callback, QVariant state)
{
    ProcessAuthorizationRequestResultCallbackCaller::call(callback, state, getResult());
}

AuthorizationResultCode::AuthorizationResultCodeEnum ProcessAuthorizationRequestResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "ProcessAuthorizationRequestResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void ProcessAuthorizationRequestResult::setResult(AuthorizationResultCode::AuthorizationResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "ProcessAuthorizationRequestResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 ProcessAuthorizationRequestResult::getRepresentedTypeId()
{
    return 10016;
}

qint32 ProcessAuthorizationRequestResult::getTypeId() const
{
    return 10016;
}
void ProcessAuthorizationRequestResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20006, this->d->result);
}
PropertyReadResult ProcessAuthorizationRequestResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = (AuthorizationResultCode::AuthorizationResultCodeEnum)reader->readEnum(20006);
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
