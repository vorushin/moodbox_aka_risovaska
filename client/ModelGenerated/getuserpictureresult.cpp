#include "listwrapperobjects.h"
#include "getuserpictureresult.h"

namespace MoodBox
{

GetUserPictureResultData::GetUserPictureResultData() : QSharedData()
{
}
GetUserPictureResultData::GetUserPictureResultData(UserPictureResult result) : QSharedData()
{
    this->result = result;
}

GetUserPictureResultData::~GetUserPictureResultData()
{
}

GetUserPictureResult::GetUserPictureResult() : TransportableObject()
{
}
GetUserPictureResult::GetUserPictureResult(UserPictureResult result) : TransportableObject()
{
    d = new GetUserPictureResultData(result);
}

GetUserPictureResult::~GetUserPictureResult()
{
}

void GetUserPictureResult::resultCall(Callback callback, QVariant state)
{
    GetUserPictureResultCallbackCaller::call(callback, state, getResult());
}

UserPictureResult GetUserPictureResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "GetUserPictureResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void GetUserPictureResult::setResult(UserPictureResult value)
{
    Q_ASSERT_X(!isNull(), "GetUserPictureResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 GetUserPictureResult::getRepresentedTypeId()
{
    return 10010;
}

qint32 GetUserPictureResult::getTypeId() const
{
    return 10010;
}
void GetUserPictureResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult GetUserPictureResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = UserPictureResult::empty();
            return PropertyReadResult(&this->d->result);
    }

    return PropertyReadResult(false);
}

}
