#include "listwrapperobjects.h"
#include "advancedsearchcontactsresult.h"

namespace MoodBox
{

AdvancedSearchContactsResultData::AdvancedSearchContactsResultData() : QSharedData()
{
}
AdvancedSearchContactsResultData::AdvancedSearchContactsResultData(UserSearchResult result) : QSharedData()
{
    this->result = result;
}

AdvancedSearchContactsResultData::~AdvancedSearchContactsResultData()
{
}

AdvancedSearchContactsResult::AdvancedSearchContactsResult() : TransportableObject()
{
}
AdvancedSearchContactsResult::AdvancedSearchContactsResult(UserSearchResult result) : TransportableObject()
{
    d = new AdvancedSearchContactsResultData(result);
}

AdvancedSearchContactsResult::~AdvancedSearchContactsResult()
{
}

void AdvancedSearchContactsResult::resultCall(Callback callback, QVariant state)
{
    AdvancedSearchContactsResultCallbackCaller::call(callback, state, getResult());
}

UserSearchResult AdvancedSearchContactsResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "AdvancedSearchContactsResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void AdvancedSearchContactsResult::setResult(UserSearchResult value)
{
    Q_ASSERT_X(!isNull(), "AdvancedSearchContactsResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 AdvancedSearchContactsResult::getRepresentedTypeId()
{
    return 10036;
}

qint32 AdvancedSearchContactsResult::getTypeId() const
{
    return 10036;
}
void AdvancedSearchContactsResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult AdvancedSearchContactsResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = UserSearchResult::empty();
            return PropertyReadResult(&this->d->result);
    }

    return PropertyReadResult(false);
}

}
