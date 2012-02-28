#include "listwrapperobjects.h"
#include "unblockcontactresult.h"

namespace MoodBox
{

UnblockContactResultData::UnblockContactResultData() : QSharedData()
{
    this->result = ContactResultCode::Ok;
}
UnblockContactResultData::UnblockContactResultData(ContactResultCode::ContactResultCodeEnum result) : QSharedData()
{
    this->result = result;
}

UnblockContactResultData::~UnblockContactResultData()
{
}

UnblockContactResult::UnblockContactResult() : TransportableObject()
{
}
UnblockContactResult::UnblockContactResult(ContactResultCode::ContactResultCodeEnum result) : TransportableObject()
{
    d = new UnblockContactResultData(result);
}

UnblockContactResult::~UnblockContactResult()
{
}

void UnblockContactResult::resultCall(Callback callback, QVariant state)
{
    UnblockContactResultCallbackCaller::call(callback, state, getResult());
}

ContactResultCode::ContactResultCodeEnum UnblockContactResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "UnblockContactResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void UnblockContactResult::setResult(ContactResultCode::ContactResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "UnblockContactResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 UnblockContactResult::getRepresentedTypeId()
{
    return 10032;
}

qint32 UnblockContactResult::getTypeId() const
{
    return 10032;
}
void UnblockContactResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20009, this->d->result);
}
PropertyReadResult UnblockContactResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
