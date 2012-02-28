#include "listwrapperobjects.h"
#include "getnotificationsresult.h"

namespace MoodBox
{

GetNotificationsResultData::GetNotificationsResultData() : QSharedData()
{
}
GetNotificationsResultData::GetNotificationsResultData(NotificationResult result) : QSharedData()
{
    this->result = result;
}

GetNotificationsResultData::~GetNotificationsResultData()
{
}

GetNotificationsResult::GetNotificationsResult() : TransportableObject()
{
}
GetNotificationsResult::GetNotificationsResult(NotificationResult result) : TransportableObject()
{
    d = new GetNotificationsResultData(result);
}

GetNotificationsResult::~GetNotificationsResult()
{
}

void GetNotificationsResult::resultCall(Callback callback, QVariant state)
{
    GetNotificationsResultCallbackCaller::call(callback, state, getResult());
}

NotificationResult GetNotificationsResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "GetNotificationsResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void GetNotificationsResult::setResult(NotificationResult value)
{
    Q_ASSERT_X(!isNull(), "GetNotificationsResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 GetNotificationsResult::getRepresentedTypeId()
{
    return 10080;
}

qint32 GetNotificationsResult::getTypeId() const
{
    return 10080;
}
void GetNotificationsResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult GetNotificationsResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = NotificationResult::empty();
            return PropertyReadResult(&this->d->result);
    }

    return PropertyReadResult(false);
}

}
