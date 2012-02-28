#include "listwrapperobjects.h"
#include "getcontactresult.h"

namespace MoodBox
{

GetContactResultData::GetContactResultData() : QSharedData()
{
}
GetContactResultData::GetContactResultData(ContactInfo result) : QSharedData()
{
    this->result = result;
}

GetContactResultData::~GetContactResultData()
{
}

GetContactResult::GetContactResult() : TransportableObject()
{
}
GetContactResult::GetContactResult(ContactInfo result) : TransportableObject()
{
    d = new GetContactResultData(result);
}

GetContactResult::~GetContactResult()
{
}

void GetContactResult::resultCall(Callback callback, QVariant state)
{
    GetContactResultCallbackCaller::call(callback, state, getResult());
}

ContactInfo GetContactResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "GetContactResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void GetContactResult::setResult(ContactInfo value)
{
    Q_ASSERT_X(!isNull(), "GetContactResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 GetContactResult::getRepresentedTypeId()
{
    return 10082;
}

qint32 GetContactResult::getTypeId() const
{
    return 10082;
}
void GetContactResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult GetContactResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = ContactInfo::empty();
            return PropertyReadResult(&this->d->result);
    }

    return PropertyReadResult(false);
}

}
