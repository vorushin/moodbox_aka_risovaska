#ifndef PUBLISHEDIMAGESRESULT_H
#define PUBLISHEDIMAGESRESULT_H

#include <QSharedData>
#include <QList>

#include "transportableobject.h"
#include "standartresultcode.h"
#include "publishedimage.h"

namespace MoodBox
{

class PublishedImagesResultData : public QSharedData
{
public:
    PublishedImagesResultData();
    PublishedImagesResultData(StandartResultCode::StandartResultCodeEnum resultCode, QList<PublishedImage> images);
    virtual ~PublishedImagesResultData();

    StandartResultCode::StandartResultCodeEnum resultCode;
    QList<PublishedImage> images;
};

class PublishedImagesResult : public TransportableObject
{
public:
    PublishedImagesResult();
    PublishedImagesResult(StandartResultCode::StandartResultCodeEnum resultCode, QList<PublishedImage> images);
    virtual ~PublishedImagesResult();

protected:
    PublishedImagesResult(PublishedImagesResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static PublishedImagesResult* ___new_()
    {
        return new PublishedImagesResult(new PublishedImagesResultData());
    }
    static PublishedImagesResult empty()
    {
        return PublishedImagesResult(new PublishedImagesResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    StandartResultCode::StandartResultCodeEnum getResultCode() const;
    void setResultCode(StandartResultCode::StandartResultCodeEnum value);
    QList<PublishedImage> getImages() const;
    void setImages(QList<PublishedImage> value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<PublishedImagesResultData> d;
};

}

#endif // PUBLISHEDIMAGESRESULT_H