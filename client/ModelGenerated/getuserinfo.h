#ifndef GETUSERINFO_H
#define GETUSERINFO_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class GetUserInfoData : public QSharedData
{
public:
    GetUserInfoData();
    GetUserInfoData(qint32 userId);
    virtual ~GetUserInfoData();

    qint32 userId;
};

class GetUserInfo : public TransportableObject
{
public:
    GetUserInfo();
    GetUserInfo(qint32 userId);
    virtual ~GetUserInfo();

protected:
    GetUserInfo(GetUserInfoData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetUserInfo* ___new_()
    {
        return new GetUserInfo(new GetUserInfoData());
    }
    static GetUserInfo empty()
    {
        return GetUserInfo(new GetUserInfoData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    qint32 getUserId() const;
    void setUserId(qint32 value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetUserInfoData> d;
};

}

#endif // GETUSERINFO_H