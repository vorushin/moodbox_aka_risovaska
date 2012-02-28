#include "listwrapperobjects.h"
#include "notificationregistrationresult.h"

namespace MoodBox
{

NotificationRegistrationResultData::NotificationRegistrationResultData() : QSharedData()
{
}
NotificationRegistrationResultData::NotificationRegistrationResultData(QString server, QString key) : QSharedData()
{
    this->server = server;
    this->key = key;
}

NotificationRegistrationResultData::~NotificationRegistrationResultData()
{
}

NotificationRegistrationResult::NotificationRegistrationResult() : TransportableObject()
{
}
NotificationRegistrationResult::NotificationRegistrationResult(QString server, QString key) : TransportableObject()
{
    d = new NotificationRegistrationResultData(server, key);
}

NotificationRegistrationResult::~NotificationRegistrationResult()
{
}

QString NotificationRegistrationResult::getServer() const
{
    Q_ASSERT_X(!isNull(), "NotificationRegistrationResult::getServer", "Getter call on object which isNull");
    return this->d->server;
}
void NotificationRegistrationResult::setServer(QString value)
{
    Q_ASSERT_X(!isNull(), "NotificationRegistrationResult::setServer", "Setter call on object which isNull");
    this->d->server = value;
}
QString NotificationRegistrationResult::getKey() const
{
    Q_ASSERT_X(!isNull(), "NotificationRegistrationResult::getKey", "Getter call on object which isNull");
    return this->d->key;
}
void NotificationRegistrationResult::setKey(QString value)
{
    Q_ASSERT_X(!isNull(), "NotificationRegistrationResult::setKey", "Setter call on object which isNull");
    this->d->key = value;
}

qint32 NotificationRegistrationResult::getRepresentedTypeId()
{
    return 17;
}

qint32 NotificationRegistrationResult::getTypeId() const
{
    return 17;
}
void NotificationRegistrationResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->server);
    writer->writeProperty(this, 2, this->d->key);
}
PropertyReadResult NotificationRegistrationResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->server = reader->readString();
            return PropertyReadResult(true);
        case 2:
            this->d->key = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
