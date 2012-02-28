#include "listwrapperobjects.h"
#include "notificationunregisterresult.h"

namespace MoodBox
{

NotificationUnregisterResultData::NotificationUnregisterResultData() : QSharedData()
{
    this->result = OkResultCode::Undefined;
}
NotificationUnregisterResultData::NotificationUnregisterResultData(OkResultCode::OkResultCodeEnum result) : QSharedData()
{
    this->result = result;
}

NotificationUnregisterResultData::~NotificationUnregisterResultData()
{
}

NotificationUnregisterResult::NotificationUnregisterResult() : TransportableObject()
{
}
NotificationUnregisterResult::NotificationUnregisterResult(OkResultCode::OkResultCodeEnum result) : TransportableObject()
{
    d = new NotificationUnregisterResultData(result);
}

NotificationUnregisterResult::~NotificationUnregisterResult()
{
}

void NotificationUnregisterResult::resultCall(Callback callback, QVariant state)
{
    NotificationUnregisterResultCallbackCaller::call(callback, state, getResult());
}

OkResultCode::OkResultCodeEnum NotificationUnregisterResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "NotificationUnregisterResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void NotificationUnregisterResult::setResult(OkResultCode::OkResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "NotificationUnregisterResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 NotificationUnregisterResult::getRepresentedTypeId()
{
    return 10078;
}

qint32 NotificationUnregisterResult::getTypeId() const
{
    return 10078;
}
void NotificationUnregisterResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20004, this->d->result);
}
PropertyReadResult NotificationUnregisterResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = (OkResultCode::OkResultCodeEnum)reader->readEnum(20004);
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
