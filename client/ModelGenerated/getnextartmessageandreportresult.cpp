#include "listwrapperobjects.h"
#include "getnextartmessageandreportresult.h"

namespace MoodBox
{

GetNextArtmessageAndReportResultData::GetNextArtmessageAndReportResultData() : QSharedData()
{
}
GetNextArtmessageAndReportResultData::GetNextArtmessageAndReportResultData(ArtMessage result) : QSharedData()
{
    this->result = result;
}

GetNextArtmessageAndReportResultData::~GetNextArtmessageAndReportResultData()
{
}

GetNextArtmessageAndReportResult::GetNextArtmessageAndReportResult() : TransportableObject()
{
}
GetNextArtmessageAndReportResult::GetNextArtmessageAndReportResult(ArtMessage result) : TransportableObject()
{
    d = new GetNextArtmessageAndReportResultData(result);
}

GetNextArtmessageAndReportResult::~GetNextArtmessageAndReportResult()
{
}

void GetNextArtmessageAndReportResult::resultCall(Callback callback, QVariant state)
{
    GetNextArtmessageAndReportResultCallbackCaller::call(callback, state, getResult());
}

ArtMessage GetNextArtmessageAndReportResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "GetNextArtmessageAndReportResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void GetNextArtmessageAndReportResult::setResult(ArtMessage value)
{
    Q_ASSERT_X(!isNull(), "GetNextArtmessageAndReportResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 GetNextArtmessageAndReportResult::getRepresentedTypeId()
{
    return 10114;
}

qint32 GetNextArtmessageAndReportResult::getTypeId() const
{
    return 10114;
}
void GetNextArtmessageAndReportResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult GetNextArtmessageAndReportResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = ArtMessage::empty();
            return PropertyReadResult(&this->d->result);
    }

    return PropertyReadResult(false);
}

}
