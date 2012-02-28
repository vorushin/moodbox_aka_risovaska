#include "listwrapperobjects.h"
#include "getcontactsresult.h"

namespace MoodBox
{

GetContactsResultData::GetContactsResultData() : QSharedData()
{
}
GetContactsResultData::GetContactsResultData(QList<ContactInfo> result) : QSharedData()
{
    this->result = result;
}

GetContactsResultData::~GetContactsResultData()
{
}

GetContactsResult::GetContactsResult() : TransportableObject()
{
}
GetContactsResult::GetContactsResult(QList<ContactInfo> result) : TransportableObject()
{
    d = new GetContactsResultData(result);
}

GetContactsResult::~GetContactsResult()
{
}

void GetContactsResult::resultCall(Callback callback, QVariant state)
{
    GetContactsResultCallbackCaller::call(callback, state, getResult());
}

QList<ContactInfo> GetContactsResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "GetContactsResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void GetContactsResult::setResult(QList<ContactInfo> value)
{
    Q_ASSERT_X(!isNull(), "GetContactsResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 GetContactsResult::getRepresentedTypeId()
{
    return 10012;
}

qint32 GetContactsResult::getTypeId() const
{
    return 10012;
}
void GetContactsResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    TransportableListOfSharedWrapper<ContactInfo> result_wrapper(this->d->result);
    writer->writeProperty(this, 1, &result_wrapper);
}
PropertyReadResult GetContactsResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = QList<ContactInfo>();
            return PropertyReadResult(new ListOfSharedWrapperObject<ContactInfo>(&this->d->result, PropertyInfo(true, false)));
    }

    return PropertyReadResult(false);
}

}
