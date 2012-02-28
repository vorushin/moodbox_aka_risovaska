#include "listwrapperobjects.h"
#include "simplesearchcontactsresult.h"

namespace MoodBox
{

SimpleSearchContactsResultData::SimpleSearchContactsResultData() : QSharedData()
{
}
SimpleSearchContactsResultData::SimpleSearchContactsResultData(UserSearchResult result) : QSharedData()
{
    this->result = result;
}

SimpleSearchContactsResultData::~SimpleSearchContactsResultData()
{
}

SimpleSearchContactsResult::SimpleSearchContactsResult() : TransportableObject()
{
}
SimpleSearchContactsResult::SimpleSearchContactsResult(UserSearchResult result) : TransportableObject()
{
    d = new SimpleSearchContactsResultData(result);
}

SimpleSearchContactsResult::~SimpleSearchContactsResult()
{
}

void SimpleSearchContactsResult::resultCall(Callback callback, QVariant state)
{
    SimpleSearchContactsResultCallbackCaller::call(callback, state, getResult());
}

UserSearchResult SimpleSearchContactsResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "SimpleSearchContactsResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void SimpleSearchContactsResult::setResult(UserSearchResult value)
{
    Q_ASSERT_X(!isNull(), "SimpleSearchContactsResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 SimpleSearchContactsResult::getRepresentedTypeId()
{
    return 10034;
}

qint32 SimpleSearchContactsResult::getTypeId() const
{
    return 10034;
}
void SimpleSearchContactsResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult SimpleSearchContactsResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
