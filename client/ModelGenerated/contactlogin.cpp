#include "listwrapperobjects.h"
#include "contactlogin.h"

namespace MoodBox
{

ContactLoginData::ContactLoginData() : QSharedData()
{
    this->contactId = 0;
}
ContactLoginData::ContactLoginData(qint32 contactId, QString login) : QSharedData()
{
    this->contactId = contactId;
    this->login = login;
}

ContactLoginData::~ContactLoginData()
{
}

ContactLogin::ContactLogin() : TransportableObject()
{
}
ContactLogin::ContactLogin(qint32 contactId, QString login) : TransportableObject()
{
    d = new ContactLoginData(contactId, login);
}

ContactLogin::~ContactLogin()
{
}

qint32 ContactLogin::getContactId() const
{
    Q_ASSERT_X(!isNull(), "ContactLogin::getContactId", "Getter call on object which isNull");
    return this->d->contactId;
}
void ContactLogin::setContactId(qint32 value)
{
    Q_ASSERT_X(!isNull(), "ContactLogin::setContactId", "Setter call on object which isNull");
    this->d->contactId = value;
}
QString ContactLogin::getLogin() const
{
    Q_ASSERT_X(!isNull(), "ContactLogin::getLogin", "Getter call on object which isNull");
    return this->d->login;
}
void ContactLogin::setLogin(QString value)
{
    Q_ASSERT_X(!isNull(), "ContactLogin::setLogin", "Setter call on object which isNull");
    this->d->login = value;
}

qint32 ContactLogin::getRepresentedTypeId()
{
    return 9;
}

qint32 ContactLogin::getTypeId() const
{
    return 9;
}
void ContactLogin::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->contactId);
    writer->writeProperty(this, 2, this->d->login);
}
PropertyReadResult ContactLogin::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->contactId = reader->readInt32();
            return PropertyReadResult(true);
        case 2:
            this->d->login = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
