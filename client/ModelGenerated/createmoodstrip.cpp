#include "listwrapperobjects.h"
#include "createmoodstrip.h"

namespace MoodBox
{

CreateMoodstripData::CreateMoodstripData() : QSharedData()
{
    this->isHidden = false;
    this->channelId = 0;
}
CreateMoodstripData::CreateMoodstripData(QString caption, bool isHidden, qint32 channelId) : QSharedData()
{
    this->caption = caption;
    this->isHidden = isHidden;
    this->channelId = channelId;
}

CreateMoodstripData::~CreateMoodstripData()
{
}

CreateMoodstrip::CreateMoodstrip() : TransportableObject()
{
}
CreateMoodstrip::CreateMoodstrip(QString caption, bool isHidden, qint32 channelId) : TransportableObject()
{
    d = new CreateMoodstripData(caption, isHidden, channelId);
}

CreateMoodstrip::~CreateMoodstrip()
{
}

QString CreateMoodstrip::getCaption() const
{
    Q_ASSERT_X(!isNull(), "CreateMoodstrip::getCaption", "Getter call on object which isNull");
    return this->d->caption;
}
void CreateMoodstrip::setCaption(QString value)
{
    Q_ASSERT_X(!isNull(), "CreateMoodstrip::setCaption", "Setter call on object which isNull");
    this->d->caption = value;
}
bool CreateMoodstrip::getIsHidden() const
{
    Q_ASSERT_X(!isNull(), "CreateMoodstrip::getIsHidden", "Getter call on object which isNull");
    return this->d->isHidden;
}
void CreateMoodstrip::setIsHidden(bool value)
{
    Q_ASSERT_X(!isNull(), "CreateMoodstrip::setIsHidden", "Setter call on object which isNull");
    this->d->isHidden = value;
}
qint32 CreateMoodstrip::getChannelId() const
{
    Q_ASSERT_X(!isNull(), "CreateMoodstrip::getChannelId", "Getter call on object which isNull");
    return this->d->channelId;
}
void CreateMoodstrip::setChannelId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "CreateMoodstrip::setChannelId", "Setter call on object which isNull");
    this->d->channelId = value;
}

qint32 CreateMoodstrip::getRepresentedTypeId()
{
    return 10045;
}

qint32 CreateMoodstrip::getTypeId() const
{
    return 10045;
}
void CreateMoodstrip::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->caption);
    writer->writeProperty(this, 2, this->d->isHidden);
    writer->writeProperty(this, 3, this->d->channelId);
}
PropertyReadResult CreateMoodstrip::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->caption = reader->readString();
            return PropertyReadResult(true);
        case 2:
            this->d->isHidden = reader->readBool();
            return PropertyReadResult(true);
        case 3:
            this->d->channelId = reader->readInt32();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
