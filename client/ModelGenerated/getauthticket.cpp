#include "listwrapperobjects.h"
#include "getauthticket.h"

namespace MoodBox
{

GetAuthTicketData::GetAuthTicketData() : QSharedData()
{
}
GetAuthTicketData::GetAuthTicketData(QString login, QString password) : QSharedData()
{
    this->login = login;
    this->password = password;
}

GetAuthTicketData::~GetAuthTicketData()
{
}

GetAuthTicket::GetAuthTicket() : TransportableObject()
{
}
GetAuthTicket::GetAuthTicket(QString login, QString password) : TransportableObject()
{
    d = new GetAuthTicketData(login, password);
}

GetAuthTicket::~GetAuthTicket()
{
}

QString GetAuthTicket::getLogin() const
{
    Q_ASSERT_X(!isNull(), "GetAuthTicket::getLogin", "Getter call on object which isNull");
    return this->d->login;
}
void GetAuthTicket::setLogin(QString value)
{
    Q_ASSERT_X(!isNull(), "GetAuthTicket::setLogin", "Setter call on object which isNull");
    this->d->login = value;
}
QString GetAuthTicket::getPassword() const
{
    Q_ASSERT_X(!isNull(), "GetAuthTicket::getPassword", "Getter call on object which isNull");
    return this->d->password;
}
void GetAuthTicket::setPassword(QString value)
{
    Q_ASSERT_X(!isNull(), "GetAuthTicket::setPassword", "Setter call on object which isNull");
    this->d->password = value;
}

qint32 GetAuthTicket::getRepresentedTypeId()
{
    return 10001;
}

qint32 GetAuthTicket::getTypeId() const
{
    return 10001;
}
void GetAuthTicket::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->login);
    writer->writeProperty(this, 2, this->d->password);
}
PropertyReadResult GetAuthTicket::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->login = reader->readString();
            return PropertyReadResult(true);
        case 2:
            this->d->password = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
