#include "listwrapperobjects.h"
#include "searchchannelresult.h"

namespace MoodBox
{

SearchChannelResultData::SearchChannelResultData() : QSharedData()
{
}
SearchChannelResultData::SearchChannelResultData(ChannelSearchResult result) : QSharedData()
{
    this->result = result;
}

SearchChannelResultData::~SearchChannelResultData()
{
}

SearchChannelResult::SearchChannelResult() : TransportableObject()
{
}
SearchChannelResult::SearchChannelResult(ChannelSearchResult result) : TransportableObject()
{
    d = new SearchChannelResultData(result);
}

SearchChannelResult::~SearchChannelResult()
{
}

void SearchChannelResult::resultCall(Callback callback, QVariant state)
{
    SearchChannelResultCallbackCaller::call(callback, state, getResult());
}

ChannelSearchResult SearchChannelResult::getResult() const
{
    Q_ASSERT_X(!isNull(), "SearchChannelResult::getResult", "Getter call on object which isNull");
    return this->d->result;
}
void SearchChannelResult::setResult(ChannelSearchResult value)
{
    Q_ASSERT_X(!isNull(), "SearchChannelResult::setResult", "Setter call on object which isNull");
    this->d->result = value;
}

qint32 SearchChannelResult::getRepresentedTypeId()
{
    return 10102;
}

qint32 SearchChannelResult::getTypeId() const
{
    return 10102;
}
void SearchChannelResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->result);
}
PropertyReadResult SearchChannelResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->result = ChannelSearchResult::empty();
            return PropertyReadResult(&this->d->result);
    }

    return PropertyReadResult(false);
}

}
