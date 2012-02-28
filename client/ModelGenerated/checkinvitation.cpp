#include "listwrapperobjects.h"
#include "checkinvitation.h"

namespace MoodBox
{

CheckInvitationData::CheckInvitationData() : QSharedData()
{
}
CheckInvitationData::CheckInvitationData(QString code) : QSharedData()
{
    this->code = code;
}

CheckInvitationData::~CheckInvitationData()
{
}

CheckInvitation::CheckInvitation() : TransportableObject()
{
}
CheckInvitation::CheckInvitation(QString code) : TransportableObject()
{
    d = new CheckInvitationData(code);
}

CheckInvitation::~CheckInvitation()
{
}

QString CheckInvitation::getCode() const
{
    Q_ASSERT_X(!isNull(), "CheckInvitation::getCode", "Getter call on object which isNull");
    return this->d->code;
}
void CheckInvitation::setCode(QString value)
{
    Q_ASSERT_X(!isNull(), "CheckInvitation::setCode", "Setter call on object which isNull");
    this->d->code = value;
}

qint32 CheckInvitation::getRepresentedTypeId()
{
    return 10091;
}

qint32 CheckInvitation::getTypeId() const
{
    return 10091;
}
void CheckInvitation::writeProperties(PropertyWriter *writer)
{
    TransportableObject::writeProperties(writer);

    writer->writeProperty(this, 1, this->d->code);
}
PropertyReadResult CheckInvitation::readProperty(qint32 propertyId, qint32 typeId, PropertyReader *reader)
{
    PropertyReadResult result = TransportableObject::readProperty(propertyId, typeId, reader);
    if(result.getIsPropertyFound())
        return result;

    switch(propertyId)
    {
        case 1:
            this->d->code = reader->readString();
            return PropertyReadResult(true);
    }

    return PropertyReadResult(false);
}

}
