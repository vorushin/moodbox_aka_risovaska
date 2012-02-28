#include "listwrapperobjects.h"
#include "deletemoodstripresult.h"

namespace MoodBox
{

DeleteMoodstripResultData::DeleteMoodstripResultData() : QSharedData()
{
    this->result = MoodstripResultCode::Ok;
}
DeleteMoodstripResultData::DeleteMoodstripResultData(MoodstripResultCode::MoodstripResultCodeEnum result) : QSharedData()
{
    this->result = result;
}

DeleteMoodstripResultData::~DeleteMoodstripResultData()
{
}

DeleteMoodstripResult::DeleteMoodstripResult() : TransportableObject()
{
}
DeleteMoodstripResult::DeleteMoodstripResult(MoodstripResultCode::MoodstripResultCodeEnum result) : TransportableObject()
{
    d = new DeleteMoodstripResultData(result);
}

DeleteMoodstripResult::~DeleteMoodstripResult()
{
}

void DeleteMoodstripResult::resultCall(Callback callback, QVariant state)
{
    DeleteMoodstripResultCallbackCaller::call(callback, state, getResult());
}

MoodstripResultCode::MoodstripResultCodeEnum DeleteMoodstripResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "DeleteMoodstripResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void DeleteMoodstripResult::setResult(MoodstripResultCode::MoodstripResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "DeleteMoodstripResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 DeleteMoodstripResult::getRepresentedTypeId()
{
    return 10052;
}

qint32 DeleteMoodstripResult::getTypeId() const
{
    return 10052;
}
void DeleteMoodstripResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20011, this->d->result);
}
PropertyReadResult DeleteMoodstripResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = (MoodstripResultCode::MoodstripResultCodeEnum)reader->readEnum(20011);
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
