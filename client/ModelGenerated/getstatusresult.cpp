#include "listwrapperobjects.h"
#include "getstatusresult.h"

namespace MoodBox
{

GetStatusResultData::GetStatusResultData() : QSharedData()
{
    this->result = UserStatus::Undefined;
}
GetStatusResultData::GetStatusResultData(UserStatus::UserStatusEnum result) : QSharedData()
{
    this->result = result;
}

GetStatusResultData::~GetStatusResultData()
{
}

GetStatusResult::GetStatusResult() : TransportableObject()
{
}
GetStatusResult::GetStatusResult(UserStatus::UserStatusEnum result) : TransportableObject()
{
    d = new GetStatusResultData(result);
}

GetStatusResult::~GetStatusResult()
{
}

void GetStatusResult::resultCall(Callback callback, QVariant state)
{
    GetStatusResultCallbackCaller::call(callback, state, getResult());
}

UserStatus::UserStatusEnum GetStatusResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "GetStatusResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void GetStatusResult::setResult(UserStatus::UserStatusEnum value)
{
    Q_ASSERT_X(!isNull(), "GetStatusResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 GetStatusResult::getRepresentedTypeId()
{
    return 10084;
}

qint32 GetStatusResult::getTypeId() const
{
    return 10084;
}
void GetStatusResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20015, this->d->result);
}
PropertyReadResult GetStatusResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = (UserStatus::UserStatusEnum)reader->readEnum(20015);
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
