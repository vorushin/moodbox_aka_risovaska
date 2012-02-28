#ifndef GETUSERPICTURE_H
#define GETUSERPICTURE_H

#include <QSharedData>
#include <QDateTime>

#include "transportableobject.h"

namespace MoodBox
{

class GetUserPictureData : public QSharedData
{
public:
    GetUserPictureData();
    GetUserPictureData(qint32 userId, QDateTime lastChangedDate);
    virtual ~GetUserPictureData();

    qint32 userId;
    QDateTime lastChangedDate;
};

class GetUserPicture : public TransportableObject
{
public:
    GetUserPicture();
    GetUserPicture(qint32 userId, QDateTime lastChangedDate);
    virtual ~GetUserPicture();

protected:
    GetUserPicture(GetUserPictureData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetUserPicture* ___new_()
    {
        return new GetUserPicture(new GetUserPictureData());
    }
    static GetUserPicture empty()
    {
        return GetUserPicture(new GetUserPictureData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getUserId() const;
    void setUserId(qint32 value);
    QDateTime getLastChangedDate() const;
    void setLastChangedDate(QDateTime value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetUserPictureData> d;
};

}

#endif // GETUSERPICTURE_H