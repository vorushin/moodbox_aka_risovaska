#include "listwrapperobjects.h"
#include "addpicturetomoodstripresult.h"

namespace MoodBox
{

AddPictureToMoodstripResultData::AddPictureToMoodstripResultData() : QSharedData()
{
}
AddPictureToMoodstripResultData::AddPictureToMoodstripResultData(PublishingMoodstripResult result) : QSharedData()
{
    this->result = result;
}

AddPictureToMoodstripResultData::~AddPictureToMoodstripResultData()
{
}

AddPictureToMoodstripResult::AddPictureToMoodstripResult() : TransportableObject()
{
}
AddPictureToMoodstripResult::AddPictureToMoodstripResult(PublishingMoodstripResult result) : TransportableObject()
{
    d = new AddPictureToMoodstripResultData(result);
}

AddPictureToMoodstripResult::~AddPictureToMoodstripResult()
{
}

void AddPictureToMoodstripResult::resultCall(Callback callback, QVariant state)
{
    AddPictureToMoodstripResultCallbackCaller::call(callback, state, getResult());
}

PublishingMoodstripResult AddPictureToMoodstripResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "AddPictureToMoodstripResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void AddPictureToMoodstripResult::setResult(PublishingMoodstripResult value)
{
    Q_ASSERT_X(!isNull(), "AddPictureToMoodstripResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 AddPictureToMoodstripResult::getRepresentedTypeId()
{
    return 10072;
}

qint32 AddPictureToMoodstripResult::getTypeId() const
{
    return 10072;
}
void AddPictureToMoodstripResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult AddPictureToMoodstripResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = PublishingMoodstripResult::empty();
            return PropertyReadResult(&this->d->result);
    }

    return PropertyReadResult(false);
}

}
