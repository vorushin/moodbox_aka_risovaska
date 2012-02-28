#ifndef USERPICTURERESULT_H
#define USERPICTURERESULT_H

#include <QSharedData>
#include <QDateTime>
#include <QByteArray>

#include "transportableobject.h"
#include "userpictureresultcode.h"

namespace MoodBox
{

class UserPictureResultData : public QSharedData
{
public:
    UserPictureResultData();
    UserPictureResultData(UserPictureResultCode::UserPictureResultCodeEnum resultCode, qint32 userId, QDateTime lastChangeDate, QByteArray pictureData);
    virtual ~UserPictureResultData();

    UserPictureResultCode::UserPictureResultCodeEnum resultCode;
    qint32 userId;
    QDateTime lastChangeDate;
    QByteArray pictureData;
};

class UserPictureResult : public TransportableObject
{
public:
    UserPictureResult();
    UserPictureResult(UserPictureResultCode::UserPictureResultCodeEnum resultCode, qint32 userId, QDateTime lastChangeDate, QByteArray pictureData);
    virtual ~UserPictureResult();

protected:
    UserPictureResult(UserPictureResultData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static UserPictureResult* ___new_()
    {
        return new UserPictureResult(new UserPictureResultData());
    }
    static UserPictureResult empty()
    {
        return UserPictureResult(new UserPictureResultData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    UserPictureResultCode::UserPictureResultCodeEnum getResultCode() const;
    void setResultCode(UserPictureResultCode::UserPictureResultCodeEnum value);
    qint32 getUserId() const;
    void setUserId(qint32 value);
    QDateTime getLastChangeDate() const;
    void setLastChangeDate(QDateTime value);
    QByteArray getPictureData() const;
    void setPictureData(QByteArray value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<UserPictureResultData> d;
};

}

#endif // USERPICTURERESULT_H