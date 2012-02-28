#include "listwrapperobjects.h"
#include "getmyaccountresult.h"

namespace MoodBox
{

GetMyAccountResultData::GetMyAccountResultData() : QSharedData()
{
}
GetMyAccountResultData::GetMyAccountResultData(UserAccount result) : QSharedData()
{
    this->result = result;
}

GetMyAccountResultData::~GetMyAccountResultData()
{
}

GetMyAccountResult::GetMyAccountResult() : TransportableObject()
{
}
GetMyAccountResult::GetMyAccountResult(UserAccount result) : TransportableObject()
{
    d = new GetMyAccountResultData(result);
}

GetMyAccountResult::~GetMyAccountResult()
{
}

void GetMyAccountResult::resultCall(Callback callback, QVariant state)
{
    GetMyAccountResultCallbackCaller::call(callback, state, getResult());
}

UserAccount GetMyAccountResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "GetMyAccountResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void GetMyAccountResult::setResult(UserAccount value)
{
    Q_ASSERT_X(!isNull(), "GetMyAccountResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 GetMyAccountResult::getRepresentedTypeId()
{
    return 10074;
}

qint32 GetMyAccountResult::getTypeId() const
{
    return 10074;
}
void GetMyAccountResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult GetMyAccountResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = UserAccount::empty();
            return PropertyReadResult(&this->d->result);
    }

    return PropertyReadResult(false);
}

}
