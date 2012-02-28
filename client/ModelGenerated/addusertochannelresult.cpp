#include "listwrapperobjects.h"
#include "addusertochannelresult.h"

namespace MoodBox
{

AddUserToChannelResultData::AddUserToChannelResultData() : QSharedData()
{
    this->result = ChangeUserChannelResult::Undefined;
}
AddUserToChannelResultData::AddUserToChannelResultData(ChangeUserChannelResult::ChangeUserChannelResultEnum result) : QSharedData()
{
    this->result = result;
}

AddUserToChannelResultData::~AddUserToChannelResultData()
{
}

AddUserToChannelResult::AddUserToChannelResult() : TransportableObject()
{
}
AddUserToChannelResult::AddUserToChannelResult(ChangeUserChannelResult::ChangeUserChannelResultEnum result) : TransportableObject()
{
    d = new AddUserToChannelResultData(result);
}

AddUserToChannelResult::~AddUserToChannelResult()
{
}

void AddUserToChannelResult::resultCall(Callback callback, QVariant state)
{
    AddUserToChannelResultCallbackCaller::call(callback, state, getResult());
}

ChangeUserChannelResult::ChangeUserChannelResultEnum AddUserToChannelResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "AddUserToChannelResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void AddUserToChannelResult::setResult(ChangeUserChannelResult::ChangeUserChannelResultEnum value)
{
    Q_ASSERT_X(!isNull(), "AddUserToChannelResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 AddUserToChannelResult::getRepresentedTypeId()
{
    return 10110;
}

qint32 AddUserToChannelResult::getTypeId() const
{
    return 10110;
}
void AddUserToChannelResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20025, this->d->result);
}
PropertyReadResult AddUserToChannelResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = (ChangeUserChannelResult::ChangeUserChannelResultEnum)reader->readEnum(20025);
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
