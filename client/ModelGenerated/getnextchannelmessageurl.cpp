#include "listwrapperobjects.h"
#include "getnextchannelmessageurl.h"

namespace MoodBox
{

GetNextChannelMessageUrlData::GetNextChannelMessageUrlData() : QSharedData()
{
    this->channelId = 0;
    this->lastMessageId = 0;
    this->skipMessage = false;
}
GetNextChannelMessageUrlData::GetNextChannelMessageUrlData(qint32 channelId, qint32 lastMessageId, bool skipMessage) : QSharedData()
{
    this->channelId = channelId;
    this->lastMessageId = lastMessageId;
    this->skipMessage = skipMessage;
}

GetNextChannelMessageUrlData::~GetNextChannelMessageUrlData()
{
}

GetNextChannelMessageUrl::GetNextChannelMessageUrl() : TransportableObject()
{
}
GetNextChannelMessageUrl::GetNextChannelMessageUrl(qint32 channelId, qint32 lastMessageId, bool skipMessage) : TransportableObject()
{
    d = new GetNextChannelMessageUrlData(channelId, lastMessageId, skipMessage);
}

GetNextChannelMessageUrl::~GetNextChannelMessageUrl()
{
}

qint32 GetNextChannelMessageUrl::getChannelId() const
{
    Q_ASSERT_X(!isNull(), "GetNextChannelMessageUrl::getChannelId", "Getter call on object which isNull");
    return this->d->channelId;
}
void GetNextChannelMessageUrl::setChannelId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "GetNextChannelMessageUrl::setChannelId", "Setter call on object which isNull");
    this->d->channelId = value;
}
qint32 GetNextChannelMessageUrl::getLastMessageId() const
{
    Q_ASSERT_X(!isNull(), "GetNextChannelMessageUrl::getLastMessageId", "Getter call on object which isNull");
    return this->d->lastMessageId;
}
void GetNextChannelMessageUrl::setLastMessageId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "GetNextChannelMessageUrl::setLastMessageId", "Setter call on object which isNull");
    this->d->lastMessageId = value;
}
bool GetNextChannelMessageUrl::getSkipMessage() const
{
    Q_ASSERT_X(!isNull(), "GetNextChannelMessageUrl::getSkipMessage", "Getter call on object which isNull");
    return this->d->skipMessage;
}
void GetNextChannelMessageUrl::setSkipMessage(bool value)
{
    Q_ASSERT_X(!isNull(), "GetNextChannelMessageUrl::setSkipMessage", "Setter call on object which isNull");
    this->d->skipMessage = value;
}

qint32 GetNextChannelMessageUrl::getRepresentedTypeId()
{
    return 10007;
}

qint32 GetNextChannelMessageUrl::getTypeId() const
{
    return 10007;
}
void GetNextChannelMessageUrl::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->channelId);
    writer->writeProperty(this, 2, this->d->lastMessageId);
    writer->writeProperty(this, 3, this->d->skipMessage);
}
PropertyReadResult GetNextChannelMessageUrl::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->channelId = reader->readInt32();
            return PropertyReadResult(true);
        case 2:
            this->d->lastMessageId = reader->readInt32();
            return PropertyReadResult(true);
        case 3:
            this->d->skipMessage = reader->readBool();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
