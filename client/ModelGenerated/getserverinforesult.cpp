#include "listwrapperobjects.h"
#include "getserverinforesult.h"

namespace MoodBox
{

GetServerInfoResultData::GetServerInfoResultData() : QSharedData()
{
}
GetServerInfoResultData::GetServerInfoResultData(ServerInfo result) : QSharedData()
{
    this->result = result;
}

GetServerInfoResultData::~GetServerInfoResultData()
{
}

GetServerInfoResult::GetServerInfoResult() : TransportableObject()
{
}
GetServerInfoResult::GetServerInfoResult(ServerInfo result) : TransportableObject()
{
    d = new GetServerInfoResultData(result);
}

GetServerInfoResult::~GetServerInfoResult()
{
}

void GetServerInfoResult::resultCall(Callback callback, QVariant state)
{
    GetServerInfoResultCallbackCaller::call(callback, state, getResult());
}

ServerInfo GetServerInfoResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "GetServerInfoResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void GetServerInfoResult::setResult(ServerInfo value)
{
    Q_ASSERT_X(!isNull(), "GetServerInfoResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 GetServerInfoResult::getRepresentedTypeId()
{
    return 10094;
}

qint32 GetServerInfoResult::getTypeId() const
{
    return 10094;
}
void GetServerInfoResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult GetServerInfoResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = ServerInfo::empty();
            return PropertyReadResult(&this->d->result);
    }

    return PropertyReadResult(false);
}

}
