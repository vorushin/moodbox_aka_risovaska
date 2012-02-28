#ifndef GETUSERINFOBYLOGIN_H
#define GETUSERINFOBYLOGIN_H

#include <QSharedData>
#include <QString>

#include "transportableobject.h"

namespace MoodBox
{

class GetUserInfoByLoginData : public QSharedData
{
public:
    GetUserInfoByLoginData();
    GetUserInfoByLoginData(QString login);
    virtual ~GetUserInfoByLoginData();

    QString login;
};

class GetUserInfoByLogin : public TransportableObject
{
public:
    GetUserInfoByLogin();
    GetUserInfoByLogin(QString login);
    virtual ~GetUserInfoByLogin();

protected:
    GetUserInfoByLogin(GetUserInfoByLoginData* dataRef)
    {
        this->d = dataRef;
    }
public:
    // never use ___new_ in your code!!!
    static GetUserInfoByLogin* ___new_()
    {
        return new GetUserInfoByLogin(new GetUserInfoByLoginData());
    }
    static GetUserInfoByLogin empty()
    {
        return GetUserInfoByLogin(new GetUserInfoByLoginData());
    }

    virtual bool isNull() const
    {
        return !d;
    }

    QString getLogin() const;
    void setLogin(QString value);

    static qint32 getRepresentedTypeId();

    virtual qint32 getTypeId() const;
    virtual void writeProperties(PropertyWriter *writer);
    virtual PropertyReadResult readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader);

private:
    QExplicitlySharedDataPointer<GetUserInfoByLoginData> d;
};

}

#endif // GETUSERINFOBYLOGIN_H