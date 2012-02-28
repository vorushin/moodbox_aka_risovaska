#include "listwrapperobjects.h"
#include "getchannelinfo.h"

namespace MoodBox
{

GetChannelInfoData::GetChannelInfoData() : QSharedData()
{
    this->channelId = 0;
}
GetChannelInfoData::GetChannelInfoData(qint32 channelId) : QSharedData()
{
    this->channelId = channelId;
}

GetChannelInfoData::~GetChannelInfoData()
{
}

GetChannelInfo::GetChannelInfo() : TransportableObject()
{
}
GetChannelInfo::GetChannelInfo(qint32 channelId) : TransportableObject()
{
    d = new GetChannelInfoData(channelId);
}

GetChannelInfo::~GetChannelInfo()
{
}

qint32 GetChannelInfo::getChannelId() const
{
    Q_ASSERT_X(!isNull(), "GetChannelInfo::getChannelId", "Getter call on object which isNull");
    return this->d->channelId;
}
void GetChannelInfo::setChannelId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "GetChannelInfo::setChannelId", "Setter call on object which isNull");
    this->d->channelId = value;
}

qint32 GetChannelInfo::getRepresentedTypeId()
{
    return 10103;
}

qint32 GetChannelInfo::getTypeId() const
{
    return 10103;
}
void GetChannelInfo::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->channelId);
}
PropertyReadResult GetChannelInfo::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
