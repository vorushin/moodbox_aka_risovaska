#include "listwrapperobjects.h"
#include "removefromcontactsresult.h"

namespace MoodBox
{

RemoveFromContactsResultData::RemoveFromContactsResultData() : QSharedData()
{
    this->result = ContactResultCode::Ok;
}
RemoveFromContactsResultData::RemoveFromContactsResultData(ContactResultCode::ContactResultCodeEnum result) : QSharedData()
{
    this->result = result;
}

RemoveFromContactsResultData::~RemoveFromContactsResultData()
{
}

RemoveFromContactsResult::RemoveFromContactsResult() : TransportableObject()
{
}
RemoveFromContactsResult::RemoveFromContactsResult(ContactResultCode::ContactResultCodeEnum result) : TransportableObject()
{
    d = new RemoveFromContactsResultData(result);
}

RemoveFromContactsResult::~RemoveFromContactsResult()
{
}

void RemoveFromContactsResult::resultCall(Callback callback, QVariant state)
{
    RemoveFromContactsResultCallbackCaller::call(callback, state, getResult());
}

ContactResultCode::ContactResultCodeEnum RemoveFromContactsResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "RemoveFromContactsResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void RemoveFromContactsResult::setResult(ContactResultCode::ContactResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "RemoveFromContactsResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 RemoveFromContactsResult::getRepresentedTypeId()
{
    return 10020;
}

qint32 RemoveFromContactsResult::getTypeId() const
{
    return 10020;
}
void RemoveFromContactsResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20009, this->d->result);
}
PropertyReadResult RemoveFromContactsResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = (ContactResultCode::ContactResultCodeEnum)reader->readEnum(20009);
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
