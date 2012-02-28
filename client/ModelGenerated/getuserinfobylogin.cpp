#include "listwrapperobjects.h"
#include "getuserinfobylogin.h"

namespace MoodBox
{

GetUserInfoByLoginData::GetUserInfoByLoginData() : QSharedData()
{
}
GetUserInfoByLoginData::GetUserInfoByLoginData(QString login) : QSharedData()
{
    this->login = login;
}

GetUserInfoByLoginData::~GetUserInfoByLoginData()
{
}

GetUserInfoByLogin::GetUserInfoByLogin() : TransportableObject()
{
}
GetUserInfoByLogin::GetUserInfoByLogin(QString login) : TransportableObject()
{
    d = new GetUserInfoByLoginData(login);
}

GetUserInfoByLogin::~GetUserInfoByLogin()
{
}

QString GetUserInfoByLogin::getLogin() const
{
    Q_ASSERT_X(!isNull(), "GetUserInfoByLogin::getLogin", "Getter call on object which isNull");
    return this->d->login;
}
void GetUserInfoByLogin::setLogin(QString value)
{
    Q_ASSERT_X(!isNull(), "GetUserInfoByLogin::setLogin", "Setter call on object which isNull");
    this->d->login = value;
}

qint32 GetUserInfoByLogin::getRepresentedTypeId()
{
    return 10097;
}

qint32 GetUserInfoByLogin::getTypeId() const
{
    return 10097;
}
void GetUserInfoByLogin::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->login);
}
PropertyReadResult GetUserInfoByLogin::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->login = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
