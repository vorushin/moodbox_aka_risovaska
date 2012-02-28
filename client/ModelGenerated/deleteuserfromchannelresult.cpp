#include "listwrapperobjects.h"
#include "deleteuserfromchannelresult.h"

namespace MoodBox
{

DeleteUserFromChannelResultData::DeleteUserFromChannelResultData() : QSharedData()
{
    this->result = ChangeUserChannelResult::Undefined;
}
DeleteUserFromChannelResultData::DeleteUserFromChannelResultData(ChangeUserChannelResult::ChangeUserChannelResultEnum result) : QSharedData()
{
    this->result = result;
}

DeleteUserFromChannelResultData::~DeleteUserFromChannelResultData()
{
}

DeleteUserFromChannelResult::DeleteUserFromChannelResult() : TransportableObject()
{
}
DeleteUserFromChannelResult::DeleteUserFromChannelResult(ChangeUserChannelResult::ChangeUserChannelResultEnum result) : TransportableObject()
{
    d = new DeleteUserFromChannelResultData(result);
}

DeleteUserFromChannelResult::~DeleteUserFromChannelResult()
{
}

void DeleteUserFromChannelResult::resultCall(Callback callback, QVariant state)
{
    DeleteUserFromChannelResultCallbackCaller::call(callback, state, getResult());
}

ChangeUserChannelResult::ChangeUserChannelResultEnum DeleteUserFromChannelResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "DeleteUserFromChannelResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void DeleteUserFromChannelResult::setResult(ChangeUserChannelResult::ChangeUserChannelResultEnum value)
{
    Q_ASSERT_X(!isNull(), "DeleteUserFromChannelResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 DeleteUserFromChannelResult::getRepresentedTypeId()
{
    return 10112;
}

qint32 DeleteUserFromChannelResult::getTypeId() const
{
    return 10112;
}
void DeleteUserFromChannelResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20025, this->d->result);
}
PropertyReadResult DeleteUserFromChannelResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
