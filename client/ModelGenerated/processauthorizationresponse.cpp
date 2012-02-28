#include "listwrapperobjects.h"
#include "processauthorizationresponse.h"

namespace MoodBox
{

ProcessAuthorizationResponseData::ProcessAuthorizationResponseData() : QSharedData()
{
    this->recipientId = 0;
    this->isAccepted = false;
}
ProcessAuthorizationResponseData::ProcessAuthorizationResponseData(qint32 recipientId, bool isAccepted) : QSharedData()
{
    this->recipientId = recipientId;
    this->isAccepted = isAccepted;
}

ProcessAuthorizationResponseData::~ProcessAuthorizationResponseData()
{
}

ProcessAuthorizationResponse::ProcessAuthorizationResponse() : TransportableObject()
{
}
ProcessAuthorizationResponse::ProcessAuthorizationResponse(qint32 recipientId, bool isAccepted) : TransportableObject()
{
    d = new ProcessAuthorizationResponseData(recipientId, isAccepted);
}

ProcessAuthorizationResponse::~ProcessAuthorizationResponse()
{
}

qint32 ProcessAuthorizationResponse::getRecipientId() const
{
    Q_ASSERT_X(!isNull(), "ProcessAuthorizationResponse::getRecipientId", "Getter call on object which isNull");
    return this->d->recipientId;
}
void ProcessAuthorizationResponse::setRecipientId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "ProcessAuthorizationResponse::setRecipientId", "Setter call on object which isNull");
    this->d->recipientId = value;
}
bool ProcessAuthorizationResponse::getIsAccepted() const
{
    Q_ASSERT_X(!isNull(), "ProcessAuthorizationResponse::getIsAccepted", "Getter call on object which isNull");
    return this->d->isAccepted;
}
void ProcessAuthorizationResponse::setIsAccepted(bool value)
{
    Q_ASSERT_X(!isNull(), "ProcessAuthorizationResponse::setIsAccepted", "Setter call on object which isNull");
    this->d->isAccepted = value;
}

qint32 ProcessAuthorizationResponse::getRepresentedTypeId()
{
    return 10017;
}

qint32 ProcessAuthorizationResponse::getTypeId() const
{
    return 10017;
}
void ProcessAuthorizationResponse::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->recipientId);
    writer->writeProperty(this, 2, this->d->isAccepted);
}
PropertyReadResult ProcessAuthorizationResponse::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->recipientId = reader->readInt32();
            return PropertyReadResult(true);
        case 2:
            this->d->isAccepted = reader->readBool();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
