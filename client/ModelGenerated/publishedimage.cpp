#include "listwrapperobjects.h"
#include "publishedimage.h"

namespace MoodBox
{

PublishedImageData::PublishedImageData() : QSharedData()
{
}
PublishedImageData::PublishedImageData(QDateTime sendDate, QString url, QString authorLogin) : QSharedData()
{
    this->sendDate = sendDate;
    this->url = url;
    this->authorLogin = authorLogin;
}

PublishedImageData::~PublishedImageData()
{
}

PublishedImage::PublishedImage() : TransportableObject()
{
}
PublishedImage::PublishedImage(QDateTime sendDate, QString url, QString authorLogin) : TransportableObject()
{
    d = new PublishedImageData(sendDate, url, authorLogin);
}

PublishedImage::~PublishedImage()
{
}

QDateTime PublishedImage::getSendDate() const
{
    Q_ASSERT_X(!isNull(), "PublishedImage::getSendDate", "Getter call on object which isNull");
    return this->d->sendDate;
}
void PublishedImage::setSendDate(QDateTime value)
{
    Q_ASSERT_X(!isNull(), "PublishedImage::setSendDate", "Setter call on object which isNull");
    this->d->sendDate = value;
}
QString PublishedImage::getUrl() const
{
    Q_ASSERT_X(!isNull(), "PublishedImage::getUrl", "Getter call on object which isNull");
    return this->d->url;
}
void PublishedImage::setUrl(QString value)
{
    Q_ASSERT_X(!isNull(), "PublishedImage::setUrl", "Setter call on object which isNull");
    this->d->url = value;
}
QString PublishedImage::getAuthorLogin() const
{
    Q_ASSERT_X(!isNull(), "PublishedImage::getAuthorLogin", "Getter call on object which isNull");
    return this->d->authorLogin;
}
void PublishedImage::setAuthorLogin(QString value)
{
    Q_ASSERT_X(!isNull(), "PublishedImage::setAuthorLogin", "Setter call on object which isNull");
    this->d->authorLogin = value;
}

qint32 PublishedImage::getRepresentedTypeId()
{
    return 10;
}

qint32 PublishedImage::getTypeId() const
{
    return 10;
}
void PublishedImage::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->sendDate);
    writer->writeProperty(this, 2, this->d->url);
    writer->writeProperty(this, 3, this->d->authorLogin);
}
PropertyReadResult PublishedImage::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->sendDate = reader->readDateTime();
            return PropertyReadResult(true);
        case 2:
            this->d->url = reader->readString();
            return PropertyReadResult(true);
        case 3:
            this->d->authorLogin = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
