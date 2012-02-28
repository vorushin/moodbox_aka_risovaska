#include "listwrapperobjects.h"
#include "processauthorizationrequestbylogin.h"

namespace MoodBox
{

ProcessAuthorizationRequestByLoginData::ProcessAuthorizationRequestByLoginData() : QSharedData()
{
}
ProcessAuthorizationRequestByLoginData::ProcessAuthorizationRequestByLoginData(QString recipientLogin, QString authorizationMessage) : QSharedData()
{
    this->recipientLogin = recipientLogin;
    this->authorizationMessage = authorizationMessage;
}

ProcessAuthorizationRequestByLoginData::~ProcessAuthorizationRequestByLoginData()
{
}

ProcessAuthorizationRequestByLogin::ProcessAuthorizationRequestByLogin() : TransportableObject()
{
}
ProcessAuthorizationRequestByLogin::ProcessAuthorizationRequestByLogin(QString recipientLogin, QString authorizationMessage) : TransportableObject()
{
    d = new ProcessAuthorizationRequestByLoginData(recipientLogin, authorizationMessage);
}

ProcessAuthorizationRequestByLogin::~ProcessAuthorizationRequestByLogin()
{
}

QString ProcessAuthorizationRequestByLogin::getRecipientLogin() const
{
    Q_ASSERT_X(!isNull(), "ProcessAuthorizationRequestByLogin::getRecipientLogin", "Getter call on object which isNull");
    return this->d->recipientLogin;
}
void ProcessAuthorizationRequestByLogin::setRecipientLogin(QString value)
{
    Q_ASSERT_X(!isNull(), "ProcessAuthorizationRequestByLogin::setRecipientLogin", "Setter call on object which isNull");
    this->d->recipientLogin = value;
}
QString ProcessAuthorizationRequestByLogin::getAuthorizationMessage() const
{
    Q_ASSERT_X(!isNull(), "ProcessAuthorizationRequestByLogin::getAuthorizationMessage", "Getter call on object which isNull");
    return this->d->authorizationMessage;
}
void ProcessAuthorizationRequestByLogin::setAuthorizationMessage(QString value)
{
    Q_ASSERT_X(!isNull(), "ProcessAuthorizationRequestByLogin::setAuthorizationMessage", "Setter call on object which isNull");
    this->d->authorizationMessage = value;
}

qint32 ProcessAuthorizationRequestByLogin::getRepresentedTypeId()
{
    return 10099;
}

qint32 ProcessAuthorizationRequestByLogin::getTypeId() const
{
    return 10099;
}
void ProcessAuthorizationRequestByLogin::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->recipientLogin);
    writer->writeProperty(this, 2, this->d->authorizationMessage);
}
PropertyReadResult ProcessAuthorizationRequestByLogin::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->recipientLogin = reader->readString();
            return PropertyReadResult(true);
        case 2:
            this->d->authorizationMessage = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
