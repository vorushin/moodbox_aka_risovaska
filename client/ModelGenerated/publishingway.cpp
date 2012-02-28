#include "listwrapperobjects.h"
#include "publishingway.h"

namespace MoodBox
{

PublishingWayData::PublishingWayData() : QSharedData()
{
    this->code = UrlCode::Undefined;
}
PublishingWayData::PublishingWayData(UrlCode::UrlCodeEnum code, QString url) : QSharedData()
{
    this->code = code;
    this->url = url;
}

PublishingWayData::~PublishingWayData()
{
}

PublishingWay::PublishingWay() : TransportableObject()
{
}
PublishingWay::PublishingWay(UrlCode::UrlCodeEnum code, QString url) : TransportableObject()
{
    d = new PublishingWayData(code, url);
}

PublishingWay::~PublishingWay()
{
}

UrlCode::UrlCodeEnum PublishingWay::getCode() const
{
    Q_ASSERT_X(!isNull(), "PublishingWay::getCode", "Getter call on object which isNull");
    return this->d->code;
}
void PublishingWay::setCode(UrlCode::UrlCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "PublishingWay::setCode", "Setter call on object which isNull");
    this->d->code = value;
}
QString PublishingWay::getUrl() const
{
    Q_ASSERT_X(!isNull(), "PublishingWay::getUrl", "Getter call on object which isNull");
    return this->d->url;
}
void PublishingWay::setUrl(QString value)
{
    Q_ASSERT_X(!isNull(), "PublishingWay::setUrl", "Setter call on object which isNull");
    this->d->url = value;
}

qint32 PublishingWay::getRepresentedTypeId()
{
    return 32;
}

qint32 PublishingWay::getTypeId() const
{
    return 32;
}
void PublishingWay::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20026, this->d->code);
    writer->writeProperty(this, 2, this->d->url);
}
PropertyReadResult PublishingWay::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->code = (UrlCode::UrlCodeEnum)reader->readEnum(20026);
            return PropertyReadResult(true);
        case 2:
            this->d->url = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
