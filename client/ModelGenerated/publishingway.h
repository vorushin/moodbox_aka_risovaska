#ifndef PUBLISHINGWAY_H
#define PUBLISHINGWAY_H

#include <QSharedData>
#include <QString>

#include "transportableobject.h"
#include "urlcode.h"

namespace MoodBox
{

class PublishingWayData : public QSharedData
{
public:
    PublishingWayData();
    PublishingWayData(UrlCode::UrlCodeEnum code, QString url);
    virtual ~PublishingWayData();

    UrlCode::UrlCodeEnum code;
    QString url;
};

class PublishingWay : public TransportableObject
{
public:
    PublishingWay();
    PublishingWay(UrlCode::UrlCodeEnum code, QString url);
    virtual ~PublishingWay();

protected:
    PublishingWay(PublishingWayData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static PublishingWay* ___new_()
    {
        return new PublishingWay(new PublishingWayData());
    }
    static PublishingWay empty()
    {
        return PublishingWay(new PublishingWayData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    UrlCode::UrlCodeEnum getCode() const;
    void setCode(UrlCode::UrlCodeEnum value);
    QString getUrl() const;
    void setUrl(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<PublishingWayData> d;
};

}

#endif // PUBLISHINGWAY_H