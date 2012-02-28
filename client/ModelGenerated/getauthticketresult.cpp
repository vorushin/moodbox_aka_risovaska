#include "listwrapperobjects.h"
#include "getauthticketresult.h"

namespace MoodBox
{

GetAuthTicketResultData::GetAuthTicketResultData() : QSharedData()
{
}
GetAuthTicketResultData::GetAuthTicketResultData(AuthTicketResult result) : QSharedData()
{
    this->result = result;
}

GetAuthTicketResultData::~GetAuthTicketResultData()
{
}

GetAuthTicketResult::GetAuthTicketResult() : TransportableObject()
{
}
GetAuthTicketResult::GetAuthTicketResult(AuthTicketResult result) : TransportableObject()
{
    d = new GetAuthTicketResultData(result);
}

GetAuthTicketResult::~GetAuthTicketResult()
{
}

void GetAuthTicketResult::resultCall(Callback callback, QVariant state)
{
    GetAuthTicketResultCallbackCaller::call(callback, state, getResult());
}

AuthTicketResult GetAuthTicketResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "GetAuthTicketResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void GetAuthTicketResult::setResult(AuthTicketResult value)
{
    Q_ASSERT_X(!isNull(), "GetAuthTicketResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 GetAuthTicketResult::getRepresentedTypeId()
{
    return 10002;
}

qint32 GetAuthTicketResult::getTypeId() const
{
    return 10002;
}
void GetAuthTicketResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult GetAuthTicketResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = AuthTicketResult::empty();
            return PropertyReadResult(&this->d->result);
    }

    return PropertyReadResult(false);
}

}
