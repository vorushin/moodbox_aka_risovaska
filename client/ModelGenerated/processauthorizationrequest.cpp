#include "listwrapperobjects.h"
#include "processauthorizationrequest.h"

namespace MoodBox
{

ProcessAuthorizationRequestData::ProcessAuthorizationRequestData() : QSharedData()
{
    this->recipientId = 0;
}
ProcessAuthorizationRequestData::ProcessAuthorizationRequestData(qint32 recipientId, QString authorizationMessage) : QSharedData()
{
    this->recipientId = recipientId;
    this->authorizationMessage = authorizationMessage;
}

ProcessAuthorizationRequestData::~ProcessAuthorizationRequestData()
{
}

ProcessAuthorizationRequest::ProcessAuthorizationRequest() : TransportableObject()
{
}
ProcessAuthorizationRequest::ProcessAuthorizationRequest(qint32 recipientId, QString authorizationMessage) : TransportableObject()
{
    d = new ProcessAuthorizationRequestData(recipientId, authorizationMessage);
}

ProcessAuthorizationRequest::~ProcessAuthorizationRequest()
{
}

qint32 ProcessAuthorizationRequest::getRecipientId() const
{
    Q_ASSERT_X(!isNull(), "ProcessAuthorizationRequest::getRecipientId", "Getter call on object which isNull");
    return this->d->recipientId;
}
void ProcessAuthorizationRequest::setRecipientId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "ProcessAuthorizationRequest::setRecipientId", "Setter call on object which isNull");
    this->d->recipientId = value;
}
QString ProcessAuthorizationRequest::getAuthorizationMessage() const
{
    Q_ASSERT_X(!isNull(), "ProcessAuthorizationRequest::getAuthorizationMessage", "Getter call on object which isNull");
    return this->d->authorizationMessage;
}
void ProcessAuthorizationRequest::setAuthorizationMessage(QString value)
{
    Q_ASSERT_X(!isNull(), "ProcessAuthorizationRequest::setAuthorizationMessage", "Setter call on object which isNull");
    this->d->authorizationMessage = value;
}

qint32 ProcessAuthorizationRequest::getRepresentedTypeId()
{
    return 10015;
}

qint32 ProcessAuthorizationRequest::getTypeId() const
{
    return 10015;
}
void ProcessAuthorizationRequest::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->recipientId);
    writer->writeProperty(this, 2, this->d->authorizationMessage);
}
PropertyReadResult ProcessAuthorizationRequest::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
            this->d->authorizationMessage = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
