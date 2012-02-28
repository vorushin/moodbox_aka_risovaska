#include "listwrapperobjects.h"
#include "updateaccount.h"

namespace MoodBox
{

UpdateAccountData::UpdateAccountData() : QSharedData()
{
    this->hasUserPicture = false;
    this->contentType = "image/png";
}
UpdateAccountData::UpdateAccountData(UserAccount userAccount, bool hasUserPicture, QByteArray userPicture, QString contentType) : QSharedData()
{
    this->userAccount = userAccount;
    this->hasUserPicture = hasUserPicture;
    this->userPicture = userPicture;
    this->contentType = contentType;
}

UpdateAccountData::~UpdateAccountData()
{
}

UpdateAccount::UpdateAccount() : TransportableObject()
{
}
UpdateAccount::UpdateAccount(UserAccount userAccount, bool hasUserPicture, QByteArray userPicture, QString contentType) : TransportableObject()
{
    d = new UpdateAccountData(userAccount, hasUserPicture, userPicture, contentType);
}

UpdateAccount::~UpdateAccount()
{
}

UserAccount UpdateAccount::getUserAccount() const
{
    Q_ASSERT_X(!isNull(), "UpdateAccount::getUserAccount", "Getter call on object which isNull");
    return this->d->userAccount;
}
void UpdateAccount::setUserAccount(UserAccount value)
{
    Q_ASSERT_X(!isNull(), "UpdateAccount::setUserAccount", "Setter call on object which isNull");
    this->d->userAccount = value;
}
bool UpdateAccount::getHasUserPicture() const
{
    Q_ASSERT_X(!isNull(), "UpdateAccount::getHasUserPicture", "Getter call on object which isNull");
    return this->d->hasUserPicture;
}
void UpdateAccount::setHasUserPicture(bool value)
{
    Q_ASSERT_X(!isNull(), "UpdateAccount::setHasUserPicture", "Setter call on object which isNull");
    this->d->hasUserPicture = value;
}
QByteArray UpdateAccount::getUserPicture() const
{
    Q_ASSERT_X(!isNull(), "UpdateAccount::getUserPicture", "Getter call on object which isNull");
    return this->d->userPicture;
}
void UpdateAccount::setUserPicture(QByteArray value)
{
    Q_ASSERT_X(!isNull(), "UpdateAccount::setUserPicture", "Setter call on object which isNull");
    this->d->userPicture = value;
}
QString UpdateAccount::getContentType() const
{
    Q_ASSERT_X(!isNull(), "UpdateAccount::getContentType", "Getter call on object which isNull");
    return this->d->contentType;
}
void UpdateAccount::setContentType(QString value)
{
    Q_ASSERT_X(!isNull(), "UpdateAccount::setContentType", "Setter call on object which isNull");
    this->d->contentType = value;
}

qint32 UpdateAccount::getRepresentedTypeId()
{
    return 10005;
}

qint32 UpdateAccount::getTypeId() const
{
    return 10005;
}
void UpdateAccount::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, &this->d->userAccount);
    writer->writeProperty(this, 2, this->d->hasUserPicture);
    writer->writeProperty(this, 3, this->d->userPicture);
    writer->writeProperty(this, 4, this->d->contentType);
}
PropertyReadResult UpdateAccount::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
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
            this->d->hasUserPicture = reader->readBool();
            return PropertyReadResult(true);
        case 3:
            this->d->userPicture = reader->readBytes();
            return PropertyReadResult(true);
        case 4:
            this->d->contentType = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
