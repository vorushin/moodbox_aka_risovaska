#ifndef GETMYACCOUNT_H
#define GETMYACCOUNT_H

#include <QSharedData>

#include "transportableobject.h"

namespace MoodBox
{

class GetMyAccountData : public QSharedData
{
public:
    GetMyAccountData();
    virtual ~GetMyAccountData();

};

class GetMyAccount : public TransportableObject
{
public:
    GetMyAccount();
    virtual ~GetMyAccount();

protected:
    GetMyAccount(GetMyAccountData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetMyAccount* ___new_()
    {
        return new GetMyAccount(new GetMyAccountData());
    }
    static GetMyAccount empty()
    {
        return GetMyAccount(new GetMyAccountData());
    }

    virtual bool isNull() const
    {
        return !d;
    }


    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetMyAccountData> d;
};

}

#endif // GETMYACCOUNT_H