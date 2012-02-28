#include "listwrapperobjects.h"
#include "getuserinfobyloginresult.h"

namespace MoodBox
{

GetUserInfoByLoginResultData::GetUserInfoByLoginResultData() : QSharedData()
{
}
GetUserInfoByLoginResultData::GetUserInfoByLoginResultData(UserInfo result) : QSharedData()
{
    this->result = result;
}

GetUserInfoByLoginResultData::~GetUserInfoByLoginResultData()
{
}

GetUserInfoByLoginResult::GetUserInfoByLoginResult() : TransportableObject()
{
}
GetUserInfoByLoginResult::GetUserInfoByLoginResult(UserInfo result) : TransportableObject()
{
    d = new GetUserInfoByLoginResultData(result);
}

GetUserInfoByLoginResult::~GetUserInfoByLoginResult()
{
}

void GetUserInfoByLoginResult::resultCall(Callback callback, QVariant state)
{
    GetUserInfoByLoginResultCallbackCaller::call(callback, state, getResult());
}

UserInfo GetUserInfoByLoginResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "GetUserInfoByLoginResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void GetUserInfoByLoginResult::setResult(UserInfo value)
{
    Q_ASSERT_X(!isNull(), "GetUserInfoByLoginResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 GetUserInfoByLoginResult::getRepresentedTypeId()
{
    return 10098;
}

qint32 GetUserInfoByLoginResult::getTypeId() const
{
    return 10098;
}
void GetUserInfoByLoginResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult GetUserInfoByLoginResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
