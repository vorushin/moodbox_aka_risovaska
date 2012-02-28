#include "listwrapperobjects.h"
#include "checkinvitationresult.h"

namespace MoodBox
{

CheckInvitationResultData::CheckInvitationResultData() : QSharedData()
{
    this->result = InvitationResultCode::Ok;
}
CheckInvitationResultData::CheckInvitationResultData(InvitationResultCode::InvitationResultCodeEnum result) : QSharedData()
{
    this->result = result;
}

CheckInvitationResultData::~CheckInvitationResultData()
{
}

CheckInvitationResult::CheckInvitationResult() : TransportableObject()
{
}
CheckInvitationResult::CheckInvitationResult(InvitationResultCode::InvitationResultCodeEnum result) : TransportableObject()
{
    d = new CheckInvitationResultData(result);
}

CheckInvitationResult::~CheckInvitationResult()
{
}

void CheckInvitationResult::resultCall(Callback callback, QVariant state)
{
    CheckInvitationResultCallbackCaller::call(callback, state, getResult());
}

InvitationResultCode::InvitationResultCodeEnum CheckInvitationResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "CheckInvitationResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void CheckInvitationResult::setResult(InvitationResultCode::InvitationResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "CheckInvitationResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 CheckInvitationResult::getRepresentedTypeId()
{
    return 10092;
}

qint32 CheckInvitationResult::getTypeId() const
{
    return 10092;
}
void CheckInvitationResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20019, this->d->result);
}
PropertyReadResult CheckInvitationResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = (InvitationResultCode::InvitationResultCodeEnum)reader->readEnum(20019);
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
