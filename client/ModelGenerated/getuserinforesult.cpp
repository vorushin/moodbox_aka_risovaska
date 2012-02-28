#include "listwrapperobjects.h"
#include "getuserinforesult.h"

namespace MoodBox
{

GetUserInfoResultData::GetUserInfoResultData() : QSharedData()
{
}
GetUserInfoResultData::GetUserInfoResultData(UserInfo result) : QSharedData()
{
    this->result = result;
}

GetUserInfoResultData::~GetUserInfoResultData()
{
}

GetUserInfoResult::GetUserInfoResult() : TransportableObject()
{
}
GetUserInfoResult::GetUserInfoResult(UserInfo result) : TransportableObject()
{
    d = new GetUserInfoResultData(result);
}

GetUserInfoResult::~GetUserInfoResult()
{
}

void GetUserInfoResult::resultCall(Callback callback, QVariant state)
{
    GetUserInfoResultCallbackCaller::call(callback, state, getResult());
}

UserInfo GetUserInfoResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "GetUserInfoResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void GetUserInfoResult::setResult(UserInfo value)
{
    Q_ASSERT_X(!isNull(), "GetUserInfoResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 GetUserInfoResult::getRepresentedTypeId()
{
    return 10040;
}

qint32 GetUserInfoResult::getTypeId() const
{
    return 10040;
}
void GetUserInfoResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult GetUserInfoResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = UserInfo::empty();
            return PropertyReadResult(&this->d->result);
    }

    return PropertyReadResult(false);
}

}
