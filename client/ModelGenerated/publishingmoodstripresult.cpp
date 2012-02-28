#include "listwrapperobjects.h"
#include "publishingmoodstripresult.h"

namespace MoodBox
{

PublishingMoodstripResultData::PublishingMoodstripResultData() : QSharedData()
{
    this->resultCode = MoodstripResultCode::Ok;
}
PublishingMoodstripResultData::PublishingMoodstripResultData(MoodstripResultCode::MoodstripResultCodeEnum resultCode, QList<PublishingWay> urls) : QSharedData()
{
    this->resultCode = resultCode;
    this->urls = urls;
}

PublishingMoodstripResultData::~PublishingMoodstripResultData()
{
}

PublishingMoodstripResult::PublishingMoodstripResult() : TransportableObject()
{
}
PublishingMoodstripResult::PublishingMoodstripResult(MoodstripResultCode::MoodstripResultCodeEnum resultCode, QList<PublishingWay> urls) : TransportableObject()
{
    d = new PublishingMoodstripResultData(resultCode, urls);
}

PublishingMoodstripResult::~PublishingMoodstripResult()
{
}

MoodstripResultCode::MoodstripResultCodeEnum PublishingMoodstripResult::getResultCode() const
{
    Q_ASSERT_X(!isNull(), "PublishingMoodstripResult::getResultCode", "Getter call on object which isNull");
    return this->d->resultCode;
}
void PublishingMoodstripResult::setResultCode(MoodstripResultCode::MoodstripResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "PublishingMoodstripResult::setResultCode", "Setter call on object which isNull");
    this->d->resultCode = value;
}
QList<PublishingWay> PublishingMoodstripResult::getUrls() const
{
    Q_ASSERT_X(!isNull(), "PublishingMoodstripResult::getUrls", "Getter call on object which isNull");
    return this->d->urls;
}
void PublishingMoodstripResult::setUrls(QList<PublishingWay> value)
{
    Q_ASSERT_X(!isNull(), "PublishingMoodstripResult::setUrls", "Setter call on object which isNull");
    this->d->urls = value;
}

qint32 PublishingMoodstripResult::getRepresentedTypeId()
{
    return 11;
}

qint32 PublishingMoodstripResult::getTypeId() const
{
    return 11;
}
void PublishingMoodstripResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20011, this->d->resultCode);
    TransportableListOfSharedWrapper<PublishingWay> urls_wrapper(this->d->urls);
    writer->writeProperty(this, 2, &urls_wrapper);
}
PropertyReadResult PublishingMoodstripResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->resultCode = (MoodstripResultCode::MoodstripResultCodeEnum)reader->readEnum(20011);
            return PropertyReadResult(true);
        case 2:
            this->d->urls = QList<PublishingWay>();
            return PropertyReadResult(new ListOfSharedWrapperObject<PublishingWay>(&this->d->urls, PropertyInfo(true, false)));
    }

    return PropertyReadResult(false);
}

}
