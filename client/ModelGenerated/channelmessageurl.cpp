#include "listwrapperobjects.h"
#include "channelmessageurl.h"

namespace MoodBox
{

ChannelMessageUrlData::ChannelMessageUrlData() : QSharedData()
{
    this->resultCode = ArtmessageResultCode::Ok;
    this->messageId = 0;
    this->authorId = 0;
}
ChannelMessageUrlData::ChannelMessageUrlData(ArtmessageResultCode::ArtmessageResultCodeEnum resultCode, qint32 messageId, qint32 authorId, QString authorLogin, QDateTime sendDate, QString url) : QSharedData()
{
    this->resultCode = resultCode;
    this->messageId = messageId;
    this->authorId = authorId;
    this->authorLogin = authorLogin;
    this->sendDate = sendDate;
    this->url = url;
}

ChannelMessageUrlData::~ChannelMessageUrlData()
{
}

ChannelMessageUrl::ChannelMessageUrl() : TransportableObject()
{
}
ChannelMessageUrl::ChannelMessageUrl(ArtmessageResultCode::ArtmessageResultCodeEnum resultCode, qint32 messageId, qint32 authorId, QString authorLogin, QDateTime sendDate, QString url) : TransportableObject()
{
    d = new ChannelMessageUrlData(resultCode, messageId, authorId, authorLogin, sendDate, url);
}

ChannelMessageUrl::~ChannelMessageUrl()
{
}

ArtmessageResultCode::ArtmessageResultCodeEnum ChannelMessageUrl::getResultCode() const
{
    Q_ASSERT_X(!isNull(), "ChannelMessageUrl::getResultCode", "Getter call on object which isNull");
    return this->d->resultCode;
}
void ChannelMessageUrl::setResultCode(ArtmessageResultCode::ArtmessageResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "ChannelMessageUrl::setResultCode", "Setter call on object which isNull");
    this->d->resultCode = value;
}
qint32 ChannelMessageUrl::getMessageId() const
{
    Q_ASSERT_X(!isNull(), "ChannelMessageUrl::getMessageId", "Getter call on object which isNull");
    return this->d->messageId;
}
void ChannelMessageUrl::setMessageId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "ChannelMessageUrl::setMessageId", "Setter call on object which isNull");
    this->d->messageId = value;
}
qint32 ChannelMessageUrl::getAuthorId() const
{
    Q_ASSERT_X(!isNull(), "ChannelMessageUrl::getAuthorId", "Getter call on object which isNull");
    return this->d->authorId;
}
void ChannelMessageUrl::setAuthorId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "ChannelMessageUrl::setAuthorId", "Setter call on object which isNull");
    this->d->authorId = value;
}
QString ChannelMessageUrl::getAuthorLogin() const
{
    Q_ASSERT_X(!isNull(), "ChannelMessageUrl::getAuthorLogin", "Getter call on object which isNull");
    return this->d->authorLogin;
}
void ChannelMessageUrl::setAuthorLogin(QString value)
{
    Q_ASSERT_X(!isNull(), "ChannelMessageUrl::setAuthorLogin", "Setter call on object which isNull");
    this->d->authorLogin = value;
}
QDateTime ChannelMessageUrl::getSendDate() const
{
    Q_ASSERT_X(!isNull(), "ChannelMessageUrl::getSendDate", "Getter call on object which isNull");
    return this->d->sendDate;
}
void ChannelMessageUrl::setSendDate(QDateTime value)
{
    Q_ASSERT_X(!isNull(), "ChannelMessageUrl::setSendDate", "Setter call on object which isNull");
    this->d->sendDate = value;
}
QString ChannelMessageUrl::getUrl() const
{
    Q_ASSERT_X(!isNull(), "ChannelMessageUrl::getUrl", "Getter call on object which isNull");
    return this->d->url;
}
void ChannelMessageUrl::setUrl(QString value)
{
    Q_ASSERT_X(!isNull(), "ChannelMessageUrl::setUrl", "Setter call on object which isNull");
    this->d->url = value;
}

qint32 ChannelMessageUrl::getRepresentedTypeId()
{
    return 31;
}

qint32 ChannelMessageUrl::getTypeId() const
{
    return 31;
}
void ChannelMessageUrl::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20018, this->d->resultCode);
    writer->writeProperty(this, 2, this->d->messageId);
    writer->writeProperty(this, 3, this->d->authorId);
    writer->writeProperty(this, 4, this->d->authorLogin);
    writer->writeProperty(this, 5, this->d->sendDate);
    writer->writeProperty(this, 6, this->d->url);
}
PropertyReadResult ChannelMessageUrl::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->resultCode = (ArtmessageResultCode::ArtmessageResultCodeEnum)reader->readEnum(20018);
            return PropertyReadResult(true);
        case 2:
            this->d->messageId = reader->readInt32();
            return PropertyReadResult(true);
        case 3:
            this->d->authorId = reader->readInt32();
            return PropertyReadResult(true);
        case 4:
            this->d->authorLogin = reader->readString();
            return PropertyReadResult(true);
        case 5:
            this->d->sendDate = reader->readDateTime();
            return PropertyReadResult(true);
        case 6:
            this->d->url = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
