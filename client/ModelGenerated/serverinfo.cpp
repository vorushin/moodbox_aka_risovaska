#include "listwrapperobjects.h"
#include "serverinfo.h"

namespace MoodBox
{

ServerInfoData::ServerInfoData() : QSharedData()
{
    this->isInvitationRequired = false;
    this->maxRequestSize = 0;
}
ServerInfoData::ServerInfoData(bool isInvitationRequired, qint32 maxRequestSize) : QSharedData()
{
    this->isInvitationRequired = isInvitationRequired;
    this->maxRequestSize = maxRequestSize;
}

ServerInfoData::~ServerInfoData()
{
}

ServerInfo::ServerInfo() : TransportableObject()
{
}
ServerInfo::ServerInfo(bool isInvitationRequired, qint32 maxRequestSize) : TransportableObject()
{
    d = new ServerInfoData(isInvitationRequired, maxRequestSize);
}

ServerInfo::~ServerInfo()
{
}

bool ServerInfo::getIsInvitationRequired() const
{
    Q_ASSERT_X(!isNull(), "ServerInfo::getIsInvitationRequired", "Getter call on object which isNull");
    return this->d->isInvitationRequired;
}
void ServerInfo::setIsInvitationRequired(bool value)
{
    Q_ASSERT_X(!isNull(), "ServerInfo::setIsInvitationRequired", "Setter call on object which isNull");
    this->d->isInvitationRequired = value;
}
qint32 ServerInfo::getMaxRequestSize() const
{
    Q_ASSERT_X(!isNull(), "ServerInfo::getMaxRequestSize", "Getter call on object which isNull");
    return this->d->maxRequestSize;
}
void ServerInfo::setMaxRequestSize(qint32 value)
{
    Q_ASSERT_X(!isNull(), "ServerInfo::setMaxRequestSize", "Setter call on object which isNull");
    this->d->maxRequestSize = value;
}

qint32 ServerInfo::getRepresentedTypeId()
{
    return 22;
}

qint32 ServerInfo::getTypeId() const
{
    return 22;
}
void ServerInfo::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->isInvitationRequired);
    writer->writeProperty(this, 2, this->d->maxRequestSize);
}
PropertyReadResult ServerInfo::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->isInvitationRequired = reader->readBool();
            return PropertyReadResult(true);
        case 2:
            this->d->maxRequestSize = reader->readInt32();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
