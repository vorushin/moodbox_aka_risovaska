#include "listwrapperobjects.h"
#include "channelresult.h"

namespace MoodBox
{

ChannelResultData::ChannelResultData() : QSharedData()
{
    this->channelId = 0;
    this->authorId = 0;
    this->userCount = 0;
}
ChannelResultData::ChannelResultData(qint32 channelId, qint32 authorId, QString authorLogin, QDate creationDate, QString title, QString shortDescription, QString fullDescription, qint32 userCount, QString logoUrl, QList<ContactLogin> moderators) : QSharedData()
{
    this->channelId = channelId;
    this->authorId = authorId;
    this->authorLogin = authorLogin;
    this->creationDate = creationDate;
    this->title = title;
    this->shortDescription = shortDescription;
    this->fullDescription = fullDescription;
    this->userCount = userCount;
    this->logoUrl = logoUrl;
    this->moderators = moderators;
}

ChannelResultData::~ChannelResultData()
{
}

ChannelResult::ChannelResult() : TransportableObject()
{
}
ChannelResult::ChannelResult(qint32 channelId, qint32 authorId, QString authorLogin, QDate creationDate, QString title, QString shortDescription, QString fullDescription, qint32 userCount, QString logoUrl, QList<ContactLogin> moderators) : TransportableObject()
{
    d = new ChannelResultData(channelId, authorId, authorLogin, creationDate, title, shortDescription, fullDescription, userCount, logoUrl, moderators);
}

ChannelResult::~ChannelResult()
{
}

qint32 ChannelResult::getChannelId() const
{
    Q_ASSERT_X(!isNull(), "ChannelResult::getChannelId", "Getter call on object which isNull");
    return this->d->channelId;
}
void ChannelResult::setChannelId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "ChannelResult::setChannelId", "Setter call on object which isNull");
    this->d->channelId = value;
}
qint32 ChannelResult::getAuthorId() const
{
    Q_ASSERT_X(!isNull(), "ChannelResult::getAuthorId", "Getter call on object which isNull");
    return this->d->authorId;
}
void ChannelResult::setAuthorId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "ChannelResult::setAuthorId", "Setter call on object which isNull");
    this->d->authorId = value;
}
QString ChannelResult::getAuthorLogin() const
{
    Q_ASSERT_X(!isNull(), "ChannelResult::getAuthorLogin", "Getter call on object which isNull");
    return this->d->authorLogin;
}
void ChannelResult::setAuthorLogin(QString value)
{
    Q_ASSERT_X(!isNull(), "ChannelResult::setAuthorLogin", "Setter call on object which isNull");
    this->d->authorLogin = value;
}
QDate ChannelResult::getCreationDate() const
{
    Q_ASSERT_X(!isNull(), "ChannelResult::getCreationDate", "Getter call on object which isNull");
    return this->d->creationDate;
}
void ChannelResult::setCreationDate(QDate value)
{
    Q_ASSERT_X(!isNull(), "ChannelResult::setCreationDate", "Setter call on object which isNull");
    this->d->creationDate = value;
}
QString ChannelResult::getTitle() const
{
    Q_ASSERT_X(!isNull(), "ChannelResult::getTitle", "Getter call on object which isNull");
    return this->d->title;
}
void ChannelResult::setTitle(QString value)
{
    Q_ASSERT_X(!isNull(), "ChannelResult::setTitle", "Setter call on object which isNull");
    this->d->title = value;
}
QString ChannelResult::getShortDescription() const
{
    Q_ASSERT_X(!isNull(), "ChannelResult::getShortDescription", "Getter call on object which isNull");
    return this->d->shortDescription;
}
void ChannelResult::setShortDescription(QString value)
{
    Q_ASSERT_X(!isNull(), "ChannelResult::setShortDescription", "Setter call on object which isNull");
    this->d->shortDescription = value;
}
QString ChannelResult::getFullDescription() const
{
    Q_ASSERT_X(!isNull(), "ChannelResult::getFullDescription", "Getter call on object which isNull");
    return this->d->fullDescription;
}
void ChannelResult::setFullDescription(QString value)
{
    Q_ASSERT_X(!isNull(), "ChannelResult::setFullDescription", "Setter call on object which isNull");
    this->d->fullDescription = value;
}
qint32 ChannelResult::getUserCount() const
{
    Q_ASSERT_X(!isNull(), "ChannelResult::getUserCount", "Getter call on object which isNull");
    return this->d->userCount;
}
void ChannelResult::setUserCount(qint32 value)
{
    Q_ASSERT_X(!isNull(), "ChannelResult::setUserCount", "Setter call on object which isNull");
    this->d->userCount = value;
}
QString ChannelResult::getLogoUrl() const
{
    Q_ASSERT_X(!isNull(), "ChannelResult::getLogoUrl", "Getter call on object which isNull");
    return this->d->logoUrl;
}
void ChannelResult::setLogoUrl(QString value)
{
    Q_ASSERT_X(!isNull(), "ChannelResult::setLogoUrl", "Setter call on object which isNull");
    this->d->logoUrl = value;
}
QList<ContactLogin> ChannelResult::getModerators() const
{
    Q_ASSERT_X(!isNull(), "ChannelResult::getModerators", "Getter call on object which isNull");
    return this->d->moderators;
}
void ChannelResult::setModerators(QList<ContactLogin> value)
{
    Q_ASSERT_X(!isNull(), "ChannelResult::setModerators", "Setter call on object which isNull");
    this->d->moderators = value;
}

qint32 ChannelResult::getRepresentedTypeId()
{
    return 25;
}

qint32 ChannelResult::getTypeId() const
{
    return 25;
}
void ChannelResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->channelId);
    writer->writeProperty(this, 2, this->d->authorId);
    writer->writeProperty(this, 3, this->d->authorLogin);
    writer->writeProperty(this, 4, this->d->creationDate);
    writer->writeProperty(this, 5, this->d->title);
    writer->writeProperty(this, 6, this->d->shortDescription);
    writer->writeProperty(this, 7, this->d->fullDescription);
    writer->writeProperty(this, 8, this->d->userCount);
    writer->writeProperty(this, 9, this->d->logoUrl);
    TransportableListOfSharedWrapper<ContactLogin> moderators_wrapper(this->d->moderators);
    writer->writeProperty(this, 10, &moderators_wrapper);
}
PropertyReadResult ChannelResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->channelId = reader->readInt32();
            return PropertyReadResult(true);
        case 2:
            this->d->authorId = reader->readInt32();
            return PropertyReadResult(true);
        case 3:
            this->d->authorLogin = reader->readString();
            return PropertyReadResult(true);
        case 4:
            this->d->creationDate = reader->readDate();
            return PropertyReadResult(true);
        case 5:
            this->d->title = reader->readString();
            return PropertyReadResult(true);
        case 6:
            this->d->shortDescription = reader->readString();
            return PropertyReadResult(true);
        case 7:
            this->d->fullDescription = reader->readString();
            return PropertyReadResult(true);
        case 8:
            this->d->userCount = reader->readInt32();
            return PropertyReadResult(true);
        case 9:
            this->d->logoUrl = reader->readString();
            return PropertyReadResult(true);
        case 10:
            this->d->moderators = QList<ContactLogin>();
            return PropertyReadResult(new ListOfSharedWrapperObject<ContactLogin>(&this->d->moderators, PropertyInfo(true, false)));
    }

    return PropertyReadResult(false);
}

}
