#include "listwrapperobjects.h"
#include "processauthorizationrequestbyloginresult.h"

namespace MoodBox
{

ProcessAuthorizationRequestByLoginResultData::ProcessAuthorizationRequestByLoginResultData() : QSharedData()
{
    this->result = AuthorizationResultCode::Ok;
}
ProcessAuthorizationRequestByLoginResultData::ProcessAuthorizationRequestByLoginResultData(AuthorizationResultCode::AuthorizationResultCodeEnum result) : QSharedData()
{
    this->result = result;
}

ProcessAuthorizationRequestByLoginResultData::~ProcessAuthorizationRequestByLoginResultData()
{
}

ProcessAuthorizationRequestByLoginResult::ProcessAuthorizationRequestByLoginResult() : TransportableObject()
{
}
ProcessAuthorizationRequestByLoginResult::ProcessAuthorizationRequestByLoginResult(AuthorizationResultCode::AuthorizationResultCodeEnum result) : TransportableObject()
{
    d = new ProcessAuthorizationRequestByLoginResultData(result);
}

ProcessAuthorizationRequestByLoginResult::~ProcessAuthorizationRequestByLoginResult()
{
}

void ProcessAuthorizationRequestByLoginResult::resultCall(Callback callback, QVariant state)
{
    ProcessAuthorizationRequestByLoginResultCallbackCaller::call(callback, state, getResult());
}

AuthorizationResultCode::AuthorizationResultCodeEnum ProcessAuthorizationRequestByLoginResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "ProcessAuthorizationRequestByLoginResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void ProcessAuthorizationRequestByLoginResult::setResult(AuthorizationResultCode::AuthorizationResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "ProcessAuthorizationRequestByLoginResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 ProcessAuthorizationRequestByLoginResult::getRepresentedTypeId()
{
    return 10100;
}

qint32 ProcessAuthorizationRequestByLoginResult::getTypeId() const
{
    return 10100;
}
void ProcessAuthorizationRequestByLoginResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20006, this->d->result);
}
PropertyReadResult ProcessAuthorizationRequestByLoginResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
