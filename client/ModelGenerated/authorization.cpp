#include "listwrapperobjects.h"
#include "authorization.h"

namespace MoodBox
{

AuthorizationData::AuthorizationData() : QSharedData()
{
    this->state = AuthorizationState::Undefined;
}
AuthorizationData::AuthorizationData(AuthorizationState::AuthorizationStateEnum state, QString message) : QSharedData()
{
    this->state = state;
    this->message = message;
}

AuthorizationData::~AuthorizationData()
{
}

Authorization::Authorization() : TransportableObject()
{
}
Authorization::Authorization(AuthorizationState::AuthorizationStateEnum state, QString message) : TransportableObject()
{
    d = new AuthorizationData(state, message);
}

Authorization::~Authorization()
{
}

AuthorizationState::AuthorizationStateEnum Authorization::getState() const
{
    Q_ASSERT_X(!isNull(), "Authorization::getState", "Getter call on object which isNull");
    return this->d->state;
}
void Authorization::setState(AuthorizationState::AuthorizationStateEnum value)
{
    Q_ASSERT_X(!isNull(), "Authorization::setState", "Setter call on object which isNull");
    this->d->state = value;
}
QString Authorization::getMessage() const
{
    Q_ASSERT_X(!isNull(), "Authorization::getMessage", "Getter call on object which isNull");
    return this->d->message;
}
void Authorization::setMessage(QString value)
{
    Q_ASSERT_X(!isNull(), "Authorization::setMessage", "Setter call on object which isNull");
    this->d->message = value;
}

qint32 Authorization::getRepresentedTypeId()
{
    return 20;
}

qint32 Authorization::getTypeId() const
{
    return 20;
}
void Authorization::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20008, this->d->state);
    writer->writeProperty(this, 2, this->d->message);
}
PropertyReadResult Authorization::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->state = (AuthorizationState::AuthorizationStateEnum)reader->readEnum(20008);
            return PropertyReadResult(true);
        case 2:
            this->d->message = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
