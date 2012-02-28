#include "listwrapperobjects.h"
#include "deleteuserfromchannel.h"

namespace MoodBox
{

DeleteUserFromChannelData::DeleteUserFromChannelData() : QSharedData()
{
    this->channelId = 0;
}
DeleteUserFromChannelData::DeleteUserFromChannelData(qint32 channelId) : QSharedData()
{
    this->channelId = channelId;
}

DeleteUserFromChannelData::~DeleteUserFromChannelData()
{
}

DeleteUserFromChannel::DeleteUserFromChannel() : TransportableObject()
{
}
DeleteUserFromChannel::DeleteUserFromChannel(qint32 channelId) : TransportableObject()
{
    d = new DeleteUserFromChannelData(channelId);
}

DeleteUserFromChannel::~DeleteUserFromChannel()
{
}

qint32 DeleteUserFromChannel::getChannelId() const
{
    Q_ASSERT_X(!isNull(), "DeleteUserFromChannel::getChannelId", "Getter call on object which isNull");
    return this->d->channelId;
}
void DeleteUserFromChannel::setChannelId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "DeleteUserFromChannel::setChannelId", "Setter call on object which isNull");
    this->d->channelId = value;
}

qint32 DeleteUserFromChannel::getRepresentedTypeId()
{
    return 10111;
}

qint32 DeleteUserFromChannel::getTypeId() const
{
    return 10111;
}
void DeleteUserFromChannel::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->channelId);
}
PropertyReadResult DeleteUserFromChannel::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->channelId = reader->readInt32();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
