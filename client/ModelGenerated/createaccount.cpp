#include "listwrapperobjects.h"
#include "createaccount.h"

namespace MoodBox
{

CreateAccountData::CreateAccountData() : QSharedData()
{
}
CreateAccountData::CreateAccountData(UserAccount userAccount, QString inviteCode) : QSharedData()
{
    this->userAccount = userAccount;
    this->inviteCode = inviteCode;
}

CreateAccountData::~CreateAccountData()
{
}

CreateAccount::CreateAccount() : TransportableObject()
{
}
CreateAccount::CreateAccount(UserAccount userAccount, QString inviteCode) : TransportableObject()
{
    d = new CreateAccountData(userAccount, inviteCode);
}

CreateAccount::~CreateAccount()
{
}

UserAccount CreateAccount::getUserAccount() const
{
    Q_ASSERT_X(!isNull(), "CreateAccount::getUserAccount", "Getter call on object which isNull");
    return this->d->userAccount;
}
void CreateAccount::setUserAccount(UserAccount value)
{
    Q_ASSERT_X(!isNull(), "CreateAccount::setUserAccount", "Setter call on object which isNull");
    this->d->userAccount = value;
}
QString CreateAccount::getInviteCode() const
{
    Q_ASSERT_X(!isNull(), "CreateAccount::getInviteCode", "Getter call on object which isNull");
    return this->d->inviteCode;
}
void CreateAccount::setInviteCode(QString value)
{
    Q_ASSERT_X(!isNull(), "CreateAccount::setInviteCode", "Setter call on object which isNull");
    this->d->inviteCode = value;
}

qint32 CreateAccount::getRepresentedTypeId()
{
    return 10003;
}

qint32 CreateAccount::getTypeId() const
{
    return 10003;
}
void CreateAccount::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->userAccount);
    writer->writeProperty(this, 2, this->d->inviteCode);
}
PropertyReadResult CreateAccount::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->userAccount = UserAccount::empty();
            return PropertyReadResult(&this->d->userAccount);
        case 2:
            this->d->inviteCode = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
