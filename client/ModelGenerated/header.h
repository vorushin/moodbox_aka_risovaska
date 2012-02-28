#ifndef HEADER_H
#define HEADER_H

#include <QSharedData>
#include <QString>
#include <QByteArray>

#include "transportableobject.h"
#include "language.h"
#include "versiontag.h"

namespace MoodBox
{

class HeaderData : public QSharedData
{
public:
    HeaderData();
    HeaderData(QByteArray authTicket, VersionTag::VersionTagEnum versionTag, QString version, Language::LanguageEnum language);
    virtual ~HeaderData();

    QByteArray authTicket;
    VersionTag::VersionTagEnum versionTag;
    QString version;
    Language::LanguageEnum language;
};

class Header : public TransportableObject
{
public:
    Header();
    Header(QByteArray authTicket, VersionTag::VersionTagEnum versionTag, QString version, Language::LanguageEnum language);
    virtual ~Header();

protected:
    Header(HeaderData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static Header* ___new_()
    {
        return new Header(new HeaderData());
    }
    static Header empty()
    {
        return Header(new HeaderData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    QByteArray getAuthTicket() const;
    void setAuthTicket(QByteArray value);
    VersionTag::VersionTagEnum getVersionTag() const;
    void setVersionTag(VersionTag::VersionTagEnum value);
    QString getVersion() const;
    void setVersion(QString value);
    Language::LanguageEnum getLanguage() const;
    void setLanguage(Language::LanguageEnum value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<HeaderData> d;
};

}

#endif // HEADER_H