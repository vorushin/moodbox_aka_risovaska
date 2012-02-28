#include "listwrapperobjects.h"
#include "getnotificationtimeoutresult.h"

namespace MoodBox
{

GetNotificationTimeoutResultData::GetNotificationTimeoutResultData() : QSharedData()
{
    this->result = 0;
}
GetNotificationTimeoutResultData::GetNotificationTimeoutResultData(qint32 result) : QSharedData()
{
    this->result = result;
}

GetNotificationTimeoutResultData::~GetNotificationTimeoutResultData()
{
}

GetNotificationTimeoutResult::GetNotificationTimeoutResult() : TransportableObject()
{
}
GetNotificationTimeoutResult::GetNotificationTimeoutResult(qint32 result) : TransportableObject()
{
    d = new GetNotificationTimeoutResultData(result);
}

GetNotificationTimeoutResult::~GetNotificationTimeoutResult()
{
}

void GetNotificationTimeoutResult::resultCall(Callback callback, QVariant state)
{
    GetNotificationTimeoutResultCallbackCaller::call(callback, state, getResult());
}

qint32 GetNotificationTimeoutResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "GetNotificationTimeoutResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void GetNotificationTimeoutResult::setResult(qint32 value)
{
    Q_ASSERT_X(!isNull(), "GetNotificationTimeoutResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 GetNotificationTimeoutResult::getRepresentedTypeId()
{
    return 10090;
}

qint32 GetNotificationTimeoutResult::getTypeId() const
{
    return 10090;
}
void GetNotificationTimeoutResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->result);
}
PropertyReadResult GetNotificationTimeoutResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = reader->readInt32();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
