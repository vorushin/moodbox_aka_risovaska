#include "listwrapperobjects.h"
#include "processauthorizationresponseresult.h"

namespace MoodBox
{

ProcessAuthorizationResponseResultData::ProcessAuthorizationResponseResultData() : QSharedData()
{
    this->result = AuthorizationResultCode::Ok;
}
ProcessAuthorizationResponseResultData::ProcessAuthorizationResponseResultData(AuthorizationResultCode::AuthorizationResultCodeEnum result) : QSharedData()
{
    this->result = result;
}

ProcessAuthorizationResponseResultData::~ProcessAuthorizationResponseResultData()
{
}

ProcessAuthorizationResponseResult::ProcessAuthorizationResponseResult() : TransportableObject()
{
}
ProcessAuthorizationResponseResult::ProcessAuthorizationResponseResult(AuthorizationResultCode::AuthorizationResultCodeEnum result) : TransportableObject()
{
    d = new ProcessAuthorizationResponseResultData(result);
}

ProcessAuthorizationResponseResult::~ProcessAuthorizationResponseResult()
{
}

void ProcessAuthorizationResponseResult::resultCall(Callback callback, QVariant state)
{
    ProcessAuthorizationResponseResultCallbackCaller::call(callback, state, getResult());
}

AuthorizationResultCode::AuthorizationResultCodeEnum ProcessAuthorizationResponseResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "ProcessAuthorizationResponseResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void ProcessAuthorizationResponseResult::setResult(AuthorizationResultCode::AuthorizationResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "ProcessAuthorizationResponseResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 ProcessAuthorizationResponseResult::getRepresentedTypeId()
{
    return 10018;
}

qint32 ProcessAuthorizationResponseResult::getTypeId() const
{
    return 10018;
}
void ProcessAuthorizationResponseResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20006, this->d->result);
}
PropertyReadResult ProcessAuthorizationResponseResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
