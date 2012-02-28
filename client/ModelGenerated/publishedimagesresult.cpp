#include "listwrapperobjects.h"
#include "publishedimagesresult.h"

namespace MoodBox
{

PublishedImagesResultData::PublishedImagesResultData() : QSharedData()
{
    this->resultCode = StandartResultCode::Undefined;
}
PublishedImagesResultData::PublishedImagesResultData(StandartResultCode::StandartResultCodeEnum resultCode, QList<PublishedImage> images) : QSharedData()
{
    this->resultCode = resultCode;
    this->images = images;
}

PublishedImagesResultData::~PublishedImagesResultData()
{
}

PublishedImagesResult::PublishedImagesResult() : TransportableObject()
{
}
PublishedImagesResult::PublishedImagesResult(StandartResultCode::StandartResultCodeEnum resultCode, QList<PublishedImage> images) : TransportableObject()
{
    d = new PublishedImagesResultData(resultCode, images);
}

PublishedImagesResult::~PublishedImagesResult()
{
}

StandartResultCode::StandartResultCodeEnum PublishedImagesResult::getResultCode() const
{
    Q_ASSERT_X(!isNull(), "PublishedImagesResult::getResultCode", "Getter call on object which isNull");
    return this->d->resultCode;
}
void PublishedImagesResult::setResultCode(StandartResultCode::StandartResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "PublishedImagesResult::setResultCode", "Setter call on object which isNull");
    this->d->resultCode = value;
}
QList<PublishedImage> PublishedImagesResult::getImages() const
{
    Q_ASSERT_X(!isNull(), "PublishedImagesResult::getImages", "Getter call on object which isNull");
    return this->d->images;
}
void PublishedImagesResult::setImages(QList<PublishedImage> value)
{
    Q_ASSERT_X(!isNull(), "PublishedImagesResult::setImages", "Setter call on object which isNull");
    this->d->images = value;
}

qint32 PublishedImagesResult::getRepresentedTypeId()
{
    return 30;
}

qint32 PublishedImagesResult::getTypeId() const
{
    return 30;
}
void PublishedImagesResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20022, this->d->resultCode);
    TransportableListOfSharedWrapper<PublishedImage> images_wrapper(this->d->images);
    writer->writeProperty(this, 2, &images_wrapper);
}
PropertyReadResult PublishedImagesResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->resultCode = (StandartResultCode::StandartResultCodeEnum)reader->readEnum(20022);
            return PropertyReadResult(true);
        case 2:
            this->d->images = QList<PublishedImage>();
            return PropertyReadResult(new ListOfSharedWrapperObject<PublishedImage>(&this->d->images, PropertyInfo(true, false)));
    }

    return PropertyReadResult(false);
}

}
