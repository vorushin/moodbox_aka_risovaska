#include "listwrapperobjects.h"
#include "contactloginsresult.h"

namespace MoodBox
{

ContactLoginsResultData::ContactLoginsResultData() : QSharedData()
{
    this->resultCode = StandartResultCode::Undefined;
}
ContactLoginsResultData::ContactLoginsResultData(StandartResultCode::StandartResultCodeEnum resultCode, QList<ContactLogin> users, QList<ContactLogin> channels) : QSharedData()
{
    this->resultCode = resultCode;
    this->users = users;
    this->channels = channels;
}

ContactLoginsResultData::~ContactLoginsResultData()
{
}

ContactLoginsResult::ContactLoginsResult() : TransportableObject()
{
}
ContactLoginsResult::ContactLoginsResult(StandartResultCode::StandartResultCodeEnum resultCode, QList<ContactLogin> users, QList<ContactLogin> channels) : TransportableObject()
{
    d = new ContactLoginsResultData(resultCode, users, channels);
}

ContactLoginsResult::~ContactLoginsResult()
{
}

StandartResultCode::StandartResultCodeEnum ContactLoginsResult::getResultCode() const
{
    Q_ASSERT_X(!isNull(), "ContactLoginsResult::getResultCode", "Getter call on object which isNull");
    return this->d->resultCode;
}
void ContactLoginsResult::setResultCode(StandartResultCode::StandartResultCodeEnum value)
{
    Q_ASSERT_X(!isNull(), "ContactLoginsResult::setResultCode", "Setter call on object which isNull");
    this->d->resultCode = value;
}
QList<ContactLogin> ContactLoginsResult::getUsers() const
{
    Q_ASSERT_X(!isNull(), "ContactLoginsResult::getUsers", "Getter call on object which isNull");
    return this->d->users;
}
void ContactLoginsResult::setUsers(QList<ContactLogin> value)
{
    Q_ASSERT_X(!isNull(), "ContactLoginsResult::setUsers", "Setter call on object which isNull");
    this->d->users = value;
}
QList<ContactLogin> ContactLoginsResult::getChannels() const
{
    Q_ASSERT_X(!isNull(), "ContactLoginsResult::getChannels", "Getter call on object which isNull");
    return this->d->channels;
}
void ContactLoginsResult::setChannels(QList<ContactLogin> value)
{
    Q_ASSERT_X(!isNull(), "ContactLoginsResult::setChannels", "Setter call on object which isNull");
    this->d->channels = value;
}

qint32 ContactLoginsResult::getRepresentedTypeId()
{
    return 23;
}

qint32 ContactLoginsResult::getTypeId() const
{
    return 23;
}
void ContactLoginsResult::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeEnumProperty(this, 1, 20022, this->d->resultCode);
    TransportableListOfSharedWrapper<ContactLogin> users_wrapper(this->d->users);
    writer->writeProperty(this, 2, &users_wrapper);
    TransportableListOfSharedWrapper<ContactLogin> channels_wrapper(this->d->channels);
    writer->writeProperty(this, 3, &channels_wrapper);
}
PropertyReadResult ContactLoginsResult::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->resultCode = (StandartResultCode::StandartResultCodeEnum)reader->readEnum(20022);
            return PropertyReadResult(true);
        case 2:
            this->d->users = QList<ContactLogin>();
            return PropertyReadResult(new ListOfSharedWrapperObject<ContactLogin>(&this->d->users, PropertyInfo(true, false)));
        case 3:
            this->d->channels = QList<ContactLogin>();
            return PropertyReadResult(new ListOfSharedWrapperObject<ContactLogin>(&this->d->channels, PropertyInfo(true, false)));
    }

    return PropertyReadResult(false);
}

}
