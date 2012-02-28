#include "listwrapperobjects.h"
#include "resetpassword.h"

namespace MoodBox
{

ResetPasswordData::ResetPasswordData() : QSharedData()
{
}
ResetPasswordData::ResetPasswordData(QString login) : QSharedData()
{
    this->login = login;
}

ResetPasswordData::~ResetPasswordData()
{
}

ResetPassword::ResetPassword() : TransportableObject()
{
}
ResetPassword::ResetPassword(QString login) : TransportableObject()
{
    d = new ResetPasswordData(login);
}

ResetPassword::~ResetPassword()
{
}

QString ResetPassword::getLogin() const
{
    Q_ASSERT_X(!isNull(), "ResetPassword::getLogin", "Getter call on object which isNull");
    return this->d->login;
}
void ResetPassword::setLogin(QString value)
{
    Q_ASSERT_X(!isNull(), "ResetPassword::setLogin", "Setter call on object which isNull");
    this->d->login = value;
}

qint32 ResetPassword::getRepresentedTypeId()
{
    return 10063;
}

qint32 ResetPassword::getTypeId() const
{
    return 10063;
}
void ResetPassword::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->login);
}
PropertyReadResult ResetPassword::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
