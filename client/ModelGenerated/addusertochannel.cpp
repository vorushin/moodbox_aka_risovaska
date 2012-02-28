#include "listwrapperobjects.h"
#include "addusertochannel.h"

namespace MoodBox
{

AddUserToChannelData::AddUserToChannelData() : QSharedData()
{
    this->channelId = 0;
}
AddUserToChannelData::AddUserToChannelData(qint32 channelId) : QSharedData()
{
    this->channelId = channelId;
}

AddUserToChannelData::~AddUserToChannelData()
{
}

AddUserToChannel::AddUserToChannel() : TransportableObject()
{
}
AddUserToChannel::AddUserToChannel(qint32 channelId) : TransportableObject()
{
    d = new AddUserToChannelData(channelId);
}

AddUserToChannel::~AddUserToChannel()
{
}

qint32 AddUserToChannel::getChannelId() const
{
    Q_ASSERT_X(!isNull(), "AddUserToChannel::getChannelId", "Getter call on object which isNull");
    return this->d->channelId;
}
void AddUserToChannel::setChannelId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "AddUserToChannel::setChannelId", "Setter call on object which isNull");
    this->d->channelId = value;
}

qint32 AddUserToChannel::getRepresentedTypeId()
{
    return 10109;
}

qint32 AddUserToChannel::getTypeId() const
{
    return 10109;
}
void AddUserToChannel::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->channelId);
}
PropertyReadResult AddUserToChannel::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
