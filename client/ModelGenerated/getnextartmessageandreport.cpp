#include "listwrapperobjects.h"
#include "getnextartmessageandreport.h"

namespace MoodBox
{

GetNextArtmessageAndReportData::GetNextArtmessageAndReportData() : QSharedData()
{
    this->previousMessageId = 0;
}
GetNextArtmessageAndReportData::GetNextArtmessageAndReportData(qint32 previousMessageId) : QSharedData()
{
    this->previousMessageId = previousMessageId;
}

GetNextArtmessageAndReportData::~GetNextArtmessageAndReportData()
{
}

GetNextArtmessageAndReport::GetNextArtmessageAndReport() : TransportableObject()
{
}
GetNextArtmessageAndReport::GetNextArtmessageAndReport(qint32 previousMessageId) : TransportableObject()
{
    d = new GetNextArtmessageAndReportData(previousMessageId);
}

GetNextArtmessageAndReport::~GetNextArtmessageAndReport()
{
}

qint32 GetNextArtmessageAndReport::getPreviousMessageId() const
{
    Q_ASSERT_X(!isNull(), "GetNextArtmessageAndReport::getPreviousMessageId", "Getter call on object which isNull");
    return this->d->previousMessageId;
}
void GetNextArtmessageAndReport::setPreviousMessageId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "GetNextArtmessageAndReport::setPreviousMessageId", "Setter call on object which isNull");
    this->d->previousMessageId = value;
}

qint32 GetNextArtmessageAndReport::getRepresentedTypeId()
{
    return 10113;
}

qint32 GetNextArtmessageAndReport::getTypeId() const
{
    return 10113;
}
void GetNextArtmessageAndReport::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->previousMessageId);
}
PropertyReadResult GetNextArtmessageAndReport::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->previousMessageId = reader->readInt32();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
