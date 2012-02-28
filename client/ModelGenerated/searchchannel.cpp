#include "listwrapperobjects.h"
#include "searchchannel.h"

namespace MoodBox
{

SearchChannelData::SearchChannelData() : QSharedData()
{
    this->pageNumber = 0;
    this->recordsPerPage = 0;
}
SearchChannelData::SearchChannelData(qint32 pageNumber, qint32 recordsPerPage, QString value) : QSharedData()
{
    this->pageNumber = pageNumber;
    this->recordsPerPage = recordsPerPage;
    this->value = value;
}

SearchChannelData::~SearchChannelData()
{
}

SearchChannel::SearchChannel() : TransportableObject()
{
}
SearchChannel::SearchChannel(qint32 pageNumber, qint32 recordsPerPage, QString value) : TransportableObject()
{
    d = new SearchChannelData(pageNumber, recordsPerPage, value);
}

SearchChannel::~SearchChannel()
{
}

qint32 SearchChannel::getPageNumber() const
{
    Q_ASSERT_X(!isNull(), "SearchChannel::getPageNumber", "Getter call on object which isNull");
    return this->d->pageNumber;
}
void SearchChannel::setPageNumber(qint32 value)
{
    Q_ASSERT_X(!isNull(), "SearchChannel::setPageNumber", "Setter call on object which isNull");
    this->d->pageNumber = value;
}
qint32 SearchChannel::getRecordsPerPage() const
{
    Q_ASSERT_X(!isNull(), "SearchChannel::getRecordsPerPage", "Getter call on object which isNull");
    return this->d->recordsPerPage;
}
void SearchChannel::setRecordsPerPage(qint32 value)
{
    Q_ASSERT_X(!isNull(), "SearchChannel::setRecordsPerPage", "Setter call on object which isNull");
    this->d->recordsPerPage = value;
}
QString SearchChannel::getValue() const
{
    Q_ASSERT_X(!isNull(), "SearchChannel::getValue", "Getter call on object which isNull");
    return this->d->value;
}
void SearchChannel::setValue(QString value)
{
    Q_ASSERT_X(!isNull(), "SearchChannel::setValue", "Setter call on object which isNull");
    this->d->value = value;
}

qint32 SearchChannel::getRepresentedTypeId()
{
    return 10101;
}

qint32 SearchChannel::getTypeId() const
{
    return 10101;
}
void SearchChannel::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->pageNumber);
    writer->writeProperty(this, 2, this->d->recordsPerPage);
    writer->writeProperty(this, 3, this->d->value);
}
PropertyReadResult SearchChannel::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->pageNumber = reader->readInt32();
            return PropertyReadResult(true);
        case 2:
            this->d->recordsPerPage = reader->readInt32();
            return PropertyReadResult(true);
        case 3:
            this->d->value = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
