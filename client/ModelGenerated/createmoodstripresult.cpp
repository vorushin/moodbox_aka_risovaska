#include "listwrapperobjects.h"
#include "createmoodstripresult.h"

namespace MoodBox
{

CreateMoodstripResultData::CreateMoodstripResultData() : QSharedData()
{
    this->result = 0;
}
CreateMoodstripResultData::CreateMoodstripResultData(qint32 result) : QSharedData()
{
    this->result = result;
}

CreateMoodstripResultData::~CreateMoodstripResultData()
{
}

CreateMoodstripResult::CreateMoodstripResult() : TransportableObject()
{
}
CreateMoodstripResult::CreateMoodstripResult(qint32 result) : TransportableObject()
{
    d = new CreateMoodstripResultData(result);
}

CreateMoodstripResult::~CreateMoodstripResult()
{
}

void CreateMoodstripResult::resultCall(Callback callback, QVariant state)
{
    CreateMoodstripResultCallbackCaller::call(callback, state, getResult());
}

qint32 CreateMoodstripResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "CreateMoodstripResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void CreateMoodstripResult::setResult(qint32 value)
{
    Q_ASSERT_X(!isNull(), "CreateMoodstripResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 CreateMoodstripResult::getRepresentedTypeId()
{
    return 10046;
}

qint32 CreateMoodstripResult::getTypeId() const
{
    return 10046;
}
void CreateMoodstripResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->result);
}
PropertyReadResult CreateMoodstripResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = reader->readInt32();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
