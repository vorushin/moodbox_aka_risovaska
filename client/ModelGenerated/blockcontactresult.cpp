#include "listwrapperobjects.h"
#include "blockcontactresult.h"

namespace MoodBox
{

BlockContactResultData::BlockContactResultData() : QSharedData()
{
    this->result = ContactResultCode::Ok;
}
BlockContactResultData::BlockContactResultData(ContactResultCode::ContactResultCodeEnum result) : QSharedData()
{
    this->result = result;
}

BlockContactResultData::~BlockContactResultData()
{
}

BlockContactResult::BlockContactResult() : TransportableObject()
{
}
BlockContactResult::BlockContactResult(ContactResultCode::ContactResultCodeEnum result) : TransportableObject()
{
    d = new BlockContactResultData(result);
}

BlockContactResult::~BlockContactResult()
{
}

void BlockContactResult::resultCall(Callback callback, QVariant state)
{
    BlockContactResultCallbackCaller::call(callback, state, getResult());
}

ContactResultCode::ContactResultCodeEnum BlockContactResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "BlockContactResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void BlockContactResult::setResult(ContactResultCode::ContactResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "BlockContactResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 BlockContactResult::getRepresentedTypeId()
{
    return 10030;
}

qint32 BlockContactResult::getTypeId() const
{
    return 10030;
}
void BlockContactResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20009, this->d->result);
}
PropertyReadResult BlockContactResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
