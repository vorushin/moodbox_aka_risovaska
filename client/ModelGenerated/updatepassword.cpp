#include "listwrapperobjects.h"
#include "updatepassword.h"

namespace MoodBox
{

UpdatePasswordData::UpdatePasswordData() : QSharedData()
{
}
UpdatePasswordData::UpdatePasswordData(QString newPassword, QString oldPassword) : QSharedData()
{
    this->newPassword = newPassword;
    this->oldPassword = oldPassword;
}

UpdatePasswordData::~UpdatePasswordData()
{
}

UpdatePassword::UpdatePassword() : TransportableObject()
{
}
UpdatePassword::UpdatePassword(QString newPassword, QString oldPassword) : TransportableObject()
{
    d = new UpdatePasswordData(newPassword, oldPassword);
}

UpdatePassword::~UpdatePassword()
{
}

QString UpdatePassword::getNewPassword() const
{
    Q_ASSERT_X(!isNull(), "UpdatePassword::getNewPassword", "Getter call on object which isNull");
    return this->d->newPassword;
}
void UpdatePassword::setNewPassword(QString value)
{
    Q_ASSERT_X(!isNull(), "UpdatePassword::setNewPassword", "Setter call on object which isNull");
    this->d->newPassword = value;
}
QString UpdatePassword::getOldPassword() const
{
    Q_ASSERT_X(!isNull(), "UpdatePassword::getOldPassword", "Getter call on object which isNull");
    return this->d->oldPassword;
}
void UpdatePassword::setOldPassword(QString value)
{
    Q_ASSERT_X(!isNull(), "UpdatePassword::setOldPassword", "Setter call on object which isNull");
    this->d->oldPassword = value;
}

qint32 UpdatePassword::getRepresentedTypeId()
{
    return 10037;
}

qint32 UpdatePassword::getTypeId() const
{
    return 10037;
}
void UpdatePassword::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->newPassword);
    writer->writeProperty(this, 2, this->d->oldPassword);
}
PropertyReadResult UpdatePassword::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->newPassword = reader->readString();
            return PropertyReadResult(true);
        case 2:
            this->d->oldPassword = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
