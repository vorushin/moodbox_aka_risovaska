#ifndef PUBLISHEDIMAGE_H
#define PUBLISHEDIMAGE_H

#include <QSharedData>
#include <QDateTime>
#include <QString>

#include "transportableobject.h"

namespace MoodBox
{

class PublishedImageData : public QSharedData
{
public:
    PublishedImageData();
    PublishedImageData(QDateTime sendDate, QString url, QString authorLogin);
    virtual ~PublishedImageData();

    QDateTime sendDate;
    QString url;
    QString authorLogin;
};

class PublishedImage : public TransportableObject
{
public:
    PublishedImage();
    PublishedImage(QDateTime sendDate, QString url, QString authorLogin);
    virtual ~PublishedImage();

protected:
    PublishedImage(PublishedImageData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static PublishedImage* ___new_()
    {
        return new PublishedImage(new PublishedImageData());
    }
    static PublishedImage empty()
    {
        return PublishedImage(new PublishedImageData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    QDateTime getSendDate() const;
    void setSendDate(QDateTime value);
    QString getUrl() const;
    void setUrl(QString value);
    QString getAuthorLogin() const;
    void setAuthorLogin(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<PublishedImageData> d;
};

}

#endif // PUBLISHEDIMAGE_H