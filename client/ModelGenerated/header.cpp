#include "listwrapperobjects.h"
#include "header.h"

namespace MoodBox
{

HeaderData::HeaderData() : QSharedData()
{
    this->versionTag = VersionTag::Undefined;
    this->language = Language::Undefined;
}
HeaderData::HeaderData(QByteArray authTicket, VersionTag::VersionTagEnum versionTag, QString version, Language::LanguageEnum language) : QSharedData()
{
    this->authTicket = authTicket;
    this->versionTag = versionTag;
    this->version = version;
    this->language = language;
}

HeaderData::~HeaderData()
{
}

Header::Header() : TransportableObject()
{
}
Header::Header(QByteArray authTicket, VersionTag::VersionTagEnum versionTag, QString version, Language::LanguageEnum language) : TransportableObject()
{
    d = new HeaderData(authTicket, versionTag, version, language);
}

Header::~Header()
{
}

QByteArray Header::getAuthTicket() const
{
    Q_ASSERT_X(!isNull(), "Header::getAuthTicket", "Getter call on object which isNull");
    return this->d->authTicket;
}
void Header::setAuthTicket(QByteArray value)
{
    Q_ASSERT_X(!isNull(), "Header::setAuthTicket", "Setter call on object which isNull");
    this->d->authTicket = value;
}
VersionTag::VersionTagEnum Header::getVersionTag() const
{
    Q_ASSERT_X(!isNull(), "Header::getVersionTag", "Getter call on object which isNull");
    return this->d->versionTag;
}
void Header::setVersionTag(VersionTag::VersionTagEnum value)
{
    Q_ASSERT_X(!isNull(), "Header::setVersionTag", "Setter call on object which isNull");
    this->d->versionTag = value;
}
QString Header::getVersion() const
{
    Q_ASSERT_X(!isNull(), "Header::getVersion", "Getter call on object which isNull");
    return this->d->version;
}
void Header::setVersion(QString value)
{
    Q_ASSERT_X(!isNull(), "Header::setVersion", "Setter call on object which isNull");
    this->d->version = value;
}
Language::LanguageEnum Header::getLanguage() const
{
    Q_ASSERT_X(!isNull(), "Header::getLanguage", "Getter call on object which isNull");
    return this->d->language;
}
void Header::setLanguage(Language::LanguageEnum value)
{
    Q_ASSERT_X(!isNull(), "Header::setLanguage", "Setter call on object which isNull");
    this->d->language = value;
}

qint32 Header::getRepresentedTypeId()
{
    return 2;
}

qint32 Header::getTypeId() const
{
    return 2;
}
void Header::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->authTicket);
    writer->writeEnumProperty(this, 2, 20020, this->d->versionTag);
    writer->writeProperty(this, 3, this->d->version);
    writer->writeEnumProperty(this, 4, 20003, this->d->language);
}
PropertyReadResult Header::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->authTicket = reader->readBytes();
            return PropertyReadResult(true);
        case 2:
            this->d->versionTag = (VersionTag::VersionTagEnum)reader->readEnum(20020);
            return PropertyReadResult(true);
        case 3:
            this->d->version = reader->readString();
            return PropertyReadResult(true);
        case 4:
            this->d->language = (Language::LanguageEnum)reader->readEnum(20003);
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
