#include "listwrapperobjects.h"
#include "deletemoodstrip.h"

namespace MoodBox
{

DeleteMoodstripData::DeleteMoodstripData() : QSharedData()
{
    this->moodstripId = 0;
}
DeleteMoodstripData::DeleteMoodstripData(qint32 moodstripId) : QSharedData()
{
    this->moodstripId = moodstripId;
}

DeleteMoodstripData::~DeleteMoodstripData()
{
}

DeleteMoodstrip::DeleteMoodstrip() : TransportableObject()
{
}
DeleteMoodstrip::DeleteMoodstrip(qint32 moodstripId) : TransportableObject()
{
    d = new DeleteMoodstripData(moodstripId);
}

DeleteMoodstrip::~DeleteMoodstrip()
{
}

qint32 DeleteMoodstrip::getMoodstripId() const
{
    Q_ASSERT_X(!isNull(), "DeleteMoodstrip::getMoodstripId", "Getter call on object which isNull");
    return this->d->moodstripId;
}
void DeleteMoodstrip::setMoodstripId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "DeleteMoodstrip::setMoodstripId", "Setter call on object which isNull");
    this->d->moodstripId = value;
}

qint32 DeleteMoodstrip::getRepresentedTypeId()
{
    return 10051;
}

qint32 DeleteMoodstrip::getTypeId() const
{
    return 10051;
}
void DeleteMoodstrip::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->moodstripId);
}
PropertyReadResult DeleteMoodstrip::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->moodstripId = reader->readInt32();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
