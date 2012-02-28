#include "listwrapperobjects.h"
#include "notificationregisterresult.h"

namespace MoodBox
{

NotificationRegisterResultData::NotificationRegisterResultData() : QSharedData()
{
}
NotificationRegisterResultData::NotificationRegisterResultData(NotificationRegistrationResult result) : QSharedData()
{
    this->result = result;
}

NotificationRegisterResultData::~NotificationRegisterResultData()
{
}

NotificationRegisterResult::NotificationRegisterResult() : TransportableObject()
{
}
NotificationRegisterResult::NotificationRegisterResult(NotificationRegistrationResult result) : TransportableObject()
{
    d = new NotificationRegisterResultData(result);
}

NotificationRegisterResult::~NotificationRegisterResult()
{
}

void NotificationRegisterResult::resultCall(Callback callback, QVariant state)
{
    NotificationRegisterResultCallbackCaller::call(callback, state, getResult());
}

NotificationRegistrationResult NotificationRegisterResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "NotificationRegisterResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void NotificationRegisterResult::setResult(NotificationRegistrationResult value)
{
    Q_ASSERT_X(!isNull(), "NotificationRegisterResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 NotificationRegisterResult::getRepresentedTypeId()
{
    return 10076;
}

qint32 NotificationRegisterResult::getTypeId() const
{
    return 10076;
}
void NotificationRegisterResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult NotificationRegisterResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = NotificationRegistrationResult::empty();
            return PropertyReadResult(&this->d->result);
    }

    return PropertyReadResult(false);
}

}
